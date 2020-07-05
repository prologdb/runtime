package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.flatMapRemaining
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PredicateNotDynamicException
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.builtin.NativeCodeRule
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import mapIndexedToArray
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
    private val declaringModule: Module
) : DynamicPrologPredicate, DelegatableCallable {
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

    private var lastClauseForTailCall: Rule? = null
    private var tailCall: CompoundTerm? = null
    private var tailCallInitialized: Boolean = false

    /**
     * Set to true by [seal] (e.g. by `compile_predicates/1`).
     */
    @Volatile
    override var isSealed = false
        private set

    override fun seal() {
        isSealed = true
    }

    override val fulfill: PrologCallableFulfill = fulfill@{ initialArguments, invocationCtxt ->
        if (!tailCallInitialized) updateTailCall()

        val ctxt = declaringModule.deriveScopedProofSearchContext(invocationCtxt)

        val delegate = currentDelegate
        if (delegate != null) {
            return@fulfill delegate.fulfill.invoke(this, initialArguments, ctxt)
        } else {
            val lastClause = clauses.last()
            var arguments = initialArguments
            tailCallLoop@while(true) {
                clauses@for (clause in clauses) {
                    if (clause is CompoundTerm) {
                        val randomizedClauseArgs = ctxt.randomVariableScope.withRandomVariables(
                            clause.arguments,
                            VariableMapping()
                        )
                        val unification = arguments.unify(randomizedClauseArgs, ctxt.randomVariableScope)
                        unification?.variableValues?.retainAll(arguments.variables)
                        if (lastClause === clause) {
                            return@fulfill unification
                        } else {
                            unification?.let { yield(it) }
                        }
                    }
                    else if (clause is Rule) {
                        if (lastClause !== clause) {
                            clause.fulfill(this, arguments, ctxt)?.let { yield(it) }
                        } else {
                            val tailCall = tailCall
                            if (tailCall != null) {
                                val lastClauseCallPreparation = lastClauseForTailCall!!.prepareCall(arguments, ctxt)
                                                                ?: return@fulfill null
                                val lastClausePartialResults = buildLazySequence<Unification>(principal) {
                                    return@buildLazySequence lastClauseForTailCall!!.fulfillPreparedCall(
                                        this@buildLazySequence,
                                        lastClauseCallPreparation
                                    )
                                }

                                val firstPartialResult = lastClausePartialResults.tryAdvance() ?: return@fulfill null
                                if (lastClausePartialResults.state == LazySequence.State.DEPLETED) {
                                    val headRequiresSteps = clause.head.arguments.any { it !is Variable && it.variables.isNotEmpty() }
                                    if (!headRequiresSteps) {
                                        val tailCallConcrete = lastClauseCallPreparation.translateClausePart(tailCall)
                                            .substituteVariables(firstPartialResult.variableValues.asSubstitutionMapper())
                                        val tailCallArguments = lastClauseCallPreparation.untranslateResult(tailCallConcrete)?.arguments
                                        if (tailCallArguments != null) {
                                            arguments = tailCallArguments
                                            continue@tailCallLoop
                                        }
                                    }
                                }

                                val partialSolutions = buildLazySequence<Unification>(principal) {
                                    yield(firstPartialResult)
                                    yieldAllFinal(lastClausePartialResults)
                                }
                                val fullSolutions = partialSolutions.flatMapRemaining<Unification, Unification> { partialResult ->
                                    val tailCallConcrete = lastClauseCallPreparation.translateClausePart(tailCall)
                                        .substituteVariables(partialResult.variableValues.asSubstitutionMapper())

                                    return@flatMapRemaining ctxt.fulfillAttach.invoke(
                                        this,
                                        PredicateInvocationQuery(tailCallConcrete, tailCallConcrete.sourceInformation),
                                        VariableBucket()
                                    )
                                }
                                val untranslatedSolutions = fullSolutions.mapRemaining { lastClauseCallPreparation.untranslateResult(it) }
                                return@fulfill yieldAllFinal(untranslatedSolutions)
                            }
                            else {
                                return@fulfill clause.fulfill(this, arguments, ctxt)
                            }
                        }
                    }
                    else {
                        throw PrologRuntimeException("Unsupported clause type ${clause.javaClass.name} in predicate $indicator")
                    }
                }
            }

            @Suppress("UNREACHABLE_CODE") null
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

        updateTailCall()
        fireClauseAddedEvent(clause)
    }

    override val retract: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unit = { matching, ctxt ->
        if (isSealed) {
            throw PredicateNotDynamicException(indicator)
        }

        while (clauses.isNotEmpty()) {
            val clause = clauses.first()
            val clauseId = clause as? CompoundTerm ?: (clause as Rule).head
            val randomClauseIdMapping = VariableMapping()
            val randomClauseId = ctxt.randomVariableScope.withRandomVariables(clauseId, randomClauseIdMapping)
            val unification = matching.unify(randomClauseId, ctxt.randomVariableScope)
            if (unification != null) {
                clauses.remove(clause)

                updateTailCall()
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

        val event = ClauseAddedToPredicateEvent(this, clause)
        for (listener in modificationListeners) {
            listener(event)
        }
    }

    private fun fireClauseRetraced(clause: Clause) {
        if (dropDelegateOnModification) {
            currentDelegate = null
        }

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

    private fun Rule.toTailCall(): Pair<Rule, CompoundTerm>? {
        if (this is NativeCodeRule) return null

        fun Query.toTailCall(): Pair<Query, CompoundTerm>? = when(this) {
            is AndQuery,
            is OrQuery -> {
                val (queryCtor, thisGoals) = when (this) {
                    is AndQuery -> Pair(::AndQuery, goals)
                    is OrQuery -> Pair(::OrQuery, goals)
                    else -> error("unreachable code")
                }
                thisGoals.last().toTailCall()?.let { (replacementGoal, tailCall) ->
                    if (replacementGoal is AndQuery && replacementGoal.goals.isEmpty()) {
                        Pair(
                            queryCtor(thisGoals.copyOfRange(0, thisGoals.size - 1)),
                            tailCall
                        )
                    } else {
                        Pair(
                            queryCtor(thisGoals.mapIndexedToArray { goal, index -> if (index == thisGoals.lastIndex) replacementGoal else goal }),
                            tailCall
                        )
                    }
                }
            }
            is PredicateInvocationQuery -> {
                if (this.goal.arity == this@ASTPrologPredicate.arity && this.goal.functor == this@ASTPrologPredicate.functor) {
                    Pair(AndQuery(emptyArray()), this.goal)
                } else {
                    null
                }
            }
        }

        return this.query.toTailCall()?.let { (replacementQuery, tailCall) -> Pair(Rule(this.head, replacementQuery), tailCall) }
    }

    private fun updateTailCall() {
        (clauses.last() as? Rule)?.toTailCall().let {
            lastClauseForTailCall = it?.first
            tailCall = it?.second
        }

        tailCallInitialized = true
    }
}

sealed class PredicateModifiedEvent
class ClauseAddedToPredicateEvent(val predicate: PrologPredicate, val clause: Clause) : PredicateModifiedEvent()
class ClauseRetractedFromPredicateEvent(val predicate: PrologPredicate, val clause: Clause) : PredicateModifiedEvent()

typealias PredicateModificationListener = (PredicateModifiedEvent) -> Any?