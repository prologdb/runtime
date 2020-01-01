package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PredicateNotDynamicException
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.analyzation.constraint.ConstrainedTerm
import com.github.prologdb.runtime.analyzation.constraint.DeterminismLevel
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import unify
import variables
import withRandomVariables
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList

/**
 * A predicate combines all clauses of a certain functor and arity. Calling the predicate
 * attempts to call every clause.
 */
interface PrologPredicate : PrologCallable {
    /**
     * Provides AST for the `listing` and `listing/1` predicates.
     */
    val clauses: List<Clause>

    val fqIndicator: FullyQualifiedClauseIndicator
}

interface DynamicPrologPredicate : PrologPredicate {
    /**
     * Inserts the given clause at the end of the existing clause-list to be evaluated in
     * [fulfill]. See prolog `assertz/1`.
     *
     * @throws PredicateNotDynamicException if this predicate [isSealed]
     */
    @Throws(PredicateNotDynamicException::class)
    fun assertz(clause: Clause)

    /**
     * Retracts facts and rules matching the given [CompoundTerm].
     */
    val retract: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unit

    /**
     * Prevents any further modification of this predicate through [assertz] or [retract].
     */
    fun seal()

    /**
     * If this predicate is sealed (see [seal])
     */
    val isSealed: Boolean

    fun addModificationListener(listener: PredicateModificationListener)

    fun removeModificationListener(listener: PredicateModificationListener)
}

/**
 * A predicate based on existing AST that is being interpreted with every call to [fulfill].
 */
class ASTPrologPredicate(
    val indicator: ClauseIndicator,
    val declaringModule: Module
) : DynamicPrologPredicate, DelegatableCallable, BehaviourExposingPrologCallable {
    private val _clauses: MutableList<Clause> = CopyOnWriteArrayList()
    override val clauses = _clauses

    override val functor = indicator.functor
    override val arity   = indicator.arity

    override val fqIndicator = FullyQualifiedClauseIndicator(declaringModule.name, indicator)

    private val modificationListeners: MutableSet<PredicateModificationListener> = Collections.newSetFromMap(ConcurrentHashMap())

    @Volatile
    private var currentDelegate: PrologCallable? = null
    @Volatile
    private var dropDelegateOnModification: Boolean = false

    /**
     * Set to true by [seal] (e.g. by `compile_predicates/1`).
     */
    @Volatile
    override var isSealed = false
        private set

    override fun seal() {
        isSealed = true
    }

    override val fulfill: suspend LazySequenceBuilder<Unification>.(Array<out Term>, ProofSearchContext) -> Unit = { arguments, invocationCtxt ->
        val ctxt = declaringModule.deriveScopedProofSearchContext(invocationCtxt)

        val delegate = currentDelegate
        if (delegate != null) {
            delegate.fulfill.invoke(this, arguments, ctxt)
        } else {
            for (clause in clauses) {
                if (clause is CompoundTerm) {
                    val randomizedClauseArgs = ctxt.randomVariableScope.withRandomVariables(clause.arguments, VariableMapping())
                    val unification = arguments.unify(randomizedClauseArgs, ctxt.randomVariableScope)
                    if (unification != null) {
                        unification.variableValues.retainAll(arguments.variables)
                        yield(unification)
                    }
                } else if (clause is Rule) {
                    clause.fulfill(this, arguments, ctxt)
                } else {
                    throw PrologRuntimeException("Unsupported clause type ${clause.javaClass.name} in predicate $indicator")
                }
            }
        }
    }

    override fun assertz(clause: Clause) {
        if (ClauseIndicator.of(clause) != indicator) {
            throw PrologRuntimeException("Cannot add clause $clause to predicate $indicator: different indicator")
        }

        if (isSealed) {
            throw PredicateNotDynamicException(fqIndicator)
        }

        _clauses.add(clause)

        fireClauseAddedEvent(clause)
    }

    override val retract: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unit = { matching, ctxt ->
        if (isSealed) {
            throw PredicateNotDynamicException(indicator)
        }

        val iterator = _clauses.listIterator()
        while (iterator.hasNext()) {
            val clause = iterator.next()
            val clauseId = clause as? CompoundTerm ?: (clause as Rule).head
            val randomClauseIdMapping = VariableMapping()
            val randomClauseId = ctxt.randomVariableScope.withRandomVariables(clauseId, randomClauseIdMapping)
            val unification = matching.unify(randomClauseId, ctxt.randomVariableScope)
            if (unification != null) {
                iterator.remove()

                fireClauseRetraced(clause)

                unification.variableValues.retainAll(matching.variables)
                yield(unification)
            }
        }
    }

    private fun fireClauseAddedEvent(clause: Clause) {
        if (dropDelegateOnModification) {
            currentDelegate = null
        }

        behaviourAnalysisResults.clear()

        val event = ClauseAddedToPredicateEvent(this, clause)
        for (listener in modificationListeners) {
            listener(event)
        }
    }

    private fun fireClauseRetraced(clause: Clause) {
        if (dropDelegateOnModification) {
            currentDelegate = null
        }

        behaviourAnalysisResults.clear()

        val event = ClauseRetractedFromPredicateEvent(this, clause)
        for (listener in modificationListeners) {
            listener(event)
        }
    }

    override fun addModificationListener(listener: PredicateModificationListener) {
        modificationListeners.add(listener)
    }

    override fun removeModificationListener(listener: PredicateModificationListener) {
        modificationListeners.remove(listener)
    }

    override fun setDelegate(delegate: PrologCallable, dropOnModification: Boolean) {
        dropDelegateOnModification = dropOnModification
        currentDelegate = delegate
    }

    override fun dropDelegate() {
        currentDelegate = null
        dropDelegateOnModification = false
    }

    private val behaviourAnalysisResults = mutableMapOf<DeterminismLevel, List<ConstrainedTerm>?>()

    override fun conditionsForBehaviour(inRuntime: PrologRuntimeEnvironment, callingModule: Module, level: DeterminismLevel): List<ConstrainedTerm>? {
        return behaviourAnalysisResults.computeIfAbsent(level) {
            if (level != DeterminismLevel.SEMI_DETERMINISTIC) {
                return@computeIfAbsent null
            }

            val clauseConditions = clauses.flatMap { clause -> when(clause) {
                is BehaviourExposingPrologCallable -> {
                    // this is where the module scope switches, because this predicate may be defined in
                    // another module than which it is called from, and the query in rules (Rule : Clause) is executed
                    // in the module context it was declared in
                    clause.conditionsForBehaviour(inRuntime, declaringModule, level) ?: return@computeIfAbsent null
                }
                else -> return@computeIfAbsent null // all clauses must be certain for a correct result on the entire predicate
            } }

            return@computeIfAbsent if (ConstrainedTerm.areMutuallyExclusive(clauseConditions)) {
                // only one clause can ever yield one solution -> entire predicate behaves that way
                clauseConditions
            } else null
        }
    }

    override fun toString() = "${declaringModule.name}/$functor/$arity"
}

sealed class PredicateModifiedEvent
class ClauseAddedToPredicateEvent(val predicate: PrologPredicate, val clause: Clause) : PredicateModifiedEvent()
class ClauseRetractedFromPredicateEvent(val predicate: PrologPredicate, val clause: Clause) : PredicateModifiedEvent()

typealias PredicateModificationListener = (PredicateModifiedEvent) -> Any?
