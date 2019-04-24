package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.forEachRemaining
import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

/**
 * Defines boilerplate code for correctly handling [AndQuery]s and [OrQuery]s as well checking read access
 * for every predicate invocation.
 */
abstract class AbstractProofSearchContext : ProofSearchContext {
    override val fulfillAttach: suspend LazySequenceBuilder<Unification>.(Query, VariableBucket) -> Unit = { q, variables ->
        when (q) {
            is AndQuery -> fulfillAndQuery(q, variables)
            is OrQuery -> for (goal in q.goals) fulfillOrQuery(q, variables)
            is PredicateInvocationQuery -> invokePredicate(q, variables)
        }
    }

    protected suspend fun LazySequenceBuilder<Unification>.fulfillAndQuery(query: AndQuery, initialVariables: VariableBucket) {
        val substitutedGoals = query.goals
            .map { it.substituteVariables(initialVariables) }

        fulfillAllGoals(substitutedGoals, this@AbstractProofSearchContext, initialVariables.copy())
    }

    protected suspend fun LazySequenceBuilder<Unification>.fulfillAllGoals(goals: List<Query>, context: ProofSearchContext,
                                                                           vars: VariableBucket = VariableBucket()) {
        val goal = goals[0].substituteVariables(vars)

        buildLazySequence<Unification>(context.principal) {
            context.fulfillAttach(this, goal, VariableBucket())
        }
            .forEachRemaining { goalUnification ->
                val goalVars = vars.copy()
                for ((variable, value) in goalUnification.variableValues.values) {
                    if (value != null) {
                        // substitute all instantiated variables for simplicity and performance
                        val substitutedValue = value.substituteVariables(goalVars.asSubstitutionMapper())
                        if (goalVars.isInstantiated(variable)) {
                            if (goalVars[variable] != substitutedValue && goalVars[variable] != value) {
                                // instantiated to different value => no unification
                                return@forEachRemaining
                            }
                        }
                        else {
                            goalVars.instantiate(variable, substitutedValue)
                        }
                    }
                }

                if (goals.size == 1) {
                    // this was the last goal in the list and it is fulfilled
                    // the variable bucket now holds all necessary instantiations
                    yield(Unification(goalVars))
                }
                else {
                    fulfillAllGoals(goals.subList(1, goals.size), context, goalVars)
                }
            }
    }

    protected suspend fun LazySequenceBuilder<Unification>.fulfillOrQuery(query: OrQuery, initialVariables: VariableBucket) {
        for (goal in query.goals) {
            fulfillAttach(goal, initialVariables)
        }
    }

    private suspend fun LazySequenceBuilder<Unification>.invokePredicate(query: PredicateInvocationQuery, variables: VariableBucket) {
        val goal = query.goal

        val indicator = ClauseIndicator.of(goal)
        if (!authorization.mayRead(indicator)) throw PrologPermissionError("Not allowed to read $indicator")

        prologTry({ getStackTraceElementOf(goal) }) {
            doInvokePredicate(goal.substituteVariables(variables.asSubstitutionMapper()), indicator)
        }
    }

    protected abstract suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(goal: CompoundTerm, indicator: ClauseIndicator)

    protected open fun getStackTraceElementOf(goal: CompoundTerm): PrologStackTraceElement = PrologStackTraceElement(
        goal,
        if (this is HasPrologSource) sourceInformation else NullSourceInformation
    )
}
