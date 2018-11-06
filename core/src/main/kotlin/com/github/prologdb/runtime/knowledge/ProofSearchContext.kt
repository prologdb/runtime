package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.forEachRemaining
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface ProofSearchContext {
    /**
     * Used to prevent race conditions between side effects of multiple
     * simultaneous proof searches.
     */
    val principal: Principal

    /**
     * To be used by code doing work in the proof search to prevent
     * collisions between variables on different stackframes.
     */
    val randomVariableScope: RandomVariableScope

    /**
     * Starts a proof search on the given query. This method will take control of
     * the coroutine until the proof search is complete.
     */
    val fulfillAttach: suspend LazySequenceBuilder<Unification>.(Query, initialVariables: VariableBucket) -> Unit
}

class RuntimeProofSearchContext(
    override val principal: Principal,
    override val randomVariableScope: RandomVariableScope
) : ProofSearchContext {
    override val fulfillAttach: suspend LazySequenceBuilder<Unification>.(Query, VariableBucket) -> Unit = { q, variables ->
        when (q) {
            is AndQuery -> fulfillAndQuery(q, variables)
            is OrQuery  -> for (goal in q.goals) fulfillOrQuery(q, variables)
            is PredicateQuery -> fulfillPredicate(q)
            else -> throw PrologRuntimeException("Unsupported query type: ${q::javaClass.name}")
        }
    }

    private suspend fun LazySequenceBuilder<Unification>.fulfillAndQuery(query: AndQuery, initialVariables: VariableBucket) {
        val substitutedGoals = query.goals
            .map { it.substituteVariables(initialVariables) }


        fulfillAllGoals(substitutedGoals, this@RuntimeProofSearchContext, initialVariables.copy())
    }

    private suspend fun LazySequenceBuilder<Unification>.fulfillAllGoals(goals: List<Query>, context: RuntimeProofSearchContext,
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

    private suspend fun LazySequenceBuilder<Unification>.fulfillOrQuery(query: OrQuery, initialVariables: VariableBucket) {
        for (goal in query.goals) {
            fulfillAttach(goal, initialVariables)
        }
    }

    private suspend fun LazySequenceBuilder<Unification>.fulfillPredicate(query: PredicateQuery, initialVariables: VariableBucket) {
        val predicate = query.predicate

        // replace all variables in the term with random ones to prevent name collisions
        val termMappings = VariableMapping()
        val replaced = randomVariableScope.withRandomVariables(predicate, termMappings)

        return buildLazySequence<Unification>(principal) {
            for (libEntry in library.findFor(predicate)) {
                if (libEntry is Predicate) {
                    val knownPredicateReplaced = context.randomVariableScope.withRandomVariables(libEntry, VariableMapping())
                    val unification = knownPredicateReplaced.unify(replaced)
                    if (unification != null) {
                        val resolvedBucket = unification.variableValues.withVariablesResolvedFrom(termMappings)
                        resolvedBucket.retainAll(predicate.variables)
                        yield(Unification(resolvedBucket))
                    }
                }
                else {
                    libEntry.unifyWithKnowledge(this, predicate, context)
                }
            }
        }
    }
}