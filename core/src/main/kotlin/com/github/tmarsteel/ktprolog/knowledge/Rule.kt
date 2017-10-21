package com.github.tmarsteel.ktprolog.knowledge

import com.google.common.collect.BiMap
import com.google.common.collect.HashBiMap
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Variable
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.unification.VariableBucket
import kotlin.coroutines.experimental.SequenceBuilder
import kotlin.coroutines.experimental.buildSequence

class Rule(val head: Predicate, val goals: List<Predicate>) {
    fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): Sequence<Unification> {
        val predicateRandomVarsMapping = HashBiMap.create<Variable, Variable>()
        val randomPredicate = randomVariableScope.withRandomVariables(predicate, predicateRandomVarsMapping)

        val ruleRandomVarsMapping = HashBiMap.create<Variable, Variable>()
        val randomHead = randomVariableScope.withRandomVariables(head, ruleRandomVarsMapping)

        val predicateAndHeadUnification = randomHead.unify(randomPredicate)
        if (predicateAndHeadUnification == null) {
            // this rule cannot be used to fulfill the given predicate
            return Unification.NONE
        }

        val randomGoals = goals
            .map { randomVariableScope.withRandomVariables(it, ruleRandomVarsMapping) }
            .map { it.substituteVariables(predicateAndHeadUnification.variableValues.asSubstitutionMapper()) }
            as List<Predicate>

        return buildSequence {
            fulfill(randomGoals, kb, ruleRandomVarsMapping)
        }.map { unification ->
            val solutionVars = VariableBucket()

            for (randomPredicateVariable in randomPredicate.variables)
            {
                if (predicateAndHeadUnification.variableValues.isDefined(randomPredicateVariable)) {
                    solutionVars.define(randomPredicateVariable)
                    val value = predicateAndHeadUnification.variableValues[randomPredicateVariable].substituteVariables(unification.variableValues.asSubstitutionMapper())
                    solutionVars.instantiate(randomPredicateVariable, value)
                }
                else {
                    val originalVar = predicateRandomVarsMapping[randomPredicateVariable]!!
                    solutionVars.define(originalVar)
                    solutionVars.instantiate(originalVar, unification.variableValues[randomPredicateVariable])
                }
            }

            Unification(resolveAllVariables(solutionVars, predicateRandomVarsMapping))
        }
    }

    private suspend fun SequenceBuilder<Unification>.fulfill(goals: List<Predicate>, kb: KnowledgeBase,
                                                             ruleRandomVarsMapping: BiMap<Variable, Variable>,
                                                             vars: VariableBucket = VariableBucket()) {
        val goal = goals[0].substituteVariables(vars.asSubstitutionMapper())

        goalUnifications@
        for (goalUnification in kb.fulfill(goal)) {
            val goalVars = vars.copy()
            for ((variable, value) in goalUnification.variableValues.values) {
                if (!goalVars.isDefined(variable)) {
                    goalVars.define(variable)
                }

                if (value.isPresent) {
                    // substitute all instantiated variables for simplicity and performance
                    val substitutedValue = value.get().substituteVariables(goalVars.asSubstitutionMapper())
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
                fulfill(goals.subList(1, goals.lastIndex), kb, ruleRandomVarsMapping, goalVars)
            }
        }
    }
}