package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.PredicateNotDynamicException
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.library.Clause
import com.github.prologdb.runtime.knowledge.library.ClauseIndicator
import com.github.prologdb.runtime.knowledge.library.Module
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification
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
}

/**
 * A predicate based on existing AST that is being interpreted with every call to [fulfill].
 */
class ASTPrologPredicate(
    val indicator: ClauseIndicator,
    val declaringModule: Module?
) : DynamicPrologPredicate {
    private val _clauses: MutableList<Clause> = CopyOnWriteArrayList()
    override val clauses = _clauses

    override val functor = indicator.functor
    override val arity   = indicator.arity

    /**
     * Set to true by [seal] (e.g. by `compile_predicates/1`).
     */
    @Volatile
    override var isSealed = false
        private set

    override fun seal() {
        isSealed = true
    }

    override val fulfill: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unit = { goal, invocationCtxt ->
        val ctxt = declaringModule?.deriveScopedProofSearchContext(invocationCtxt) ?: invocationCtxt

        for (clause in clauses) {
            if (clause is CompoundTerm) {
                val randomizedClause = ctxt.randomVariableScope.withRandomVariables(clause, VariableMapping())
                val unification = goal.unify(randomizedClause)
                if (unification != null) {
                    unification.variableValues.retainAll(goal.variables)
                    yield(unification)
                }
            }
            else if (clause is Rule) {
                clause.unifyWithKnowledge(this, goal, ctxt)
            }
            else {
                throw PrologRuntimeException("Unsupported clause type ${clause.javaClass.name} in predicate $indicator")
            }
        }
    }

    override fun assertz(clause: Clause) {
        if (ClauseIndicator.of(clause) != indicator) {
            throw PrologRuntimeException("Cannot add clause $clause to predicate $indicator: different indicator")
        }

        if (isSealed) {
            throw PredicateNotDynamicException(indicator)
        }

        _clauses.add(clause)
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
                unification.variableValues.retainAll(matching.variables)
                yield(unification)
            }
        }
    }
}
