package com.github.prologdb.runtime.query

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import mapToArray
import kotlin.coroutines.experimental.SequenceBuilder
import kotlin.coroutines.experimental.buildSequence

open class AndQuery(val goals: Array<out Query>) : Query {
    override fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket, randomVarsScope: RandomVariableScope): Sequence<Unification> {
        val substitutedGoals = goals
            .map { it.substituteVariables(initialVariables) }

        return buildSequence {
            fulfillAllGoals(substitutedGoals, kb, randomVarsScope, initialVariables.copy())
        }
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return AndQuery(
                goals.mapToArray { it.withRandomVariables(randomVarsScope, mapping) }
        )
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return AndQuery(
                goals.mapToArray { it.substituteVariables(variableValues) }
        )
    }

    override fun toString(): String {
        return goals.mapToArray { it.toString() }.joinToString(", ")
    }

    private suspend fun SequenceBuilder<Unification>.fulfillAllGoals(goals: List<Query>, kb: KnowledgeBase,
                                                                     randomVarsScope: RandomVariableScope,
                                                                     vars: VariableBucket = VariableBucket()) {
        val goal = goals[0].substituteVariables(vars)

        goalUnifications@
        for (goalUnification in kb.fulfill(goal, randomVarsScope)) {
            val goalVars = vars.copy()
            for ((variable, value) in goalUnification.variableValues.values) {
                if (value != null) {
                    // substitute all instantiated variables for simplicity and performance
                    val substitutedValue = value.substituteVariables(goalVars.asSubstitutionMapper())
                    if (goalVars.isInstantiated(variable)) {
                        if (goalVars[variable] != substitutedValue && goalVars[variable] != value) {
                            // instantiated to different value => no unification
                            continue@goalUnifications
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
                fulfillAllGoals(goals.subList(1, goals.size), kb, randomVarsScope, goalVars)
            }
        }
    }
}