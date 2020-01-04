package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.analyzation.constraint.ConstrainedTerm
import com.github.prologdb.runtime.analyzation.constraint.DeterminismLevel
import com.github.prologdb.runtime.analyzation.constraint.ImpossibleConstraint
import com.github.prologdb.runtime.analyzation.constraint.NoopConstraint
import com.github.prologdb.runtime.analyzation.constraint.TermConstraint
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import com.github.prologdb.runtime.unification.VariableDiscrepancyException
import com.github.prologdb.runtime.util.associateWithNotNull
import unify
import variables
import withRandomVariables

open class Rule(val head: CompoundTerm, val query: Query) : Clause, BehaviourExposingPrologCallable {
    override val functor = head.functor
    override val arity = head.arity

    /**
     * Calls this rule with the given invocation goal.
     *
     * Default behaviour:
     * Randomizes all variables in the head, query and `goal`. Then unifies `goal` with
     * the head. Carries the resulting variable mappings over to the query and continues the
     * proof-search coroutine with solutions for the resulting query.
     *
     * This can be re-defined for built-ins.
     */
    override val fulfill: suspend LazySequenceBuilder<Unification>.(Array<out Term>, ProofSearchContext) -> Unit = { arguments, context ->
        val goalRandomVarsMapping = VariableMapping()
        val randomArgs = context.randomVariableScope.withRandomVariables(arguments, goalRandomVarsMapping)

        val ruleRandomVarsMapping = VariableMapping()
        val randomHeadArgs = context.randomVariableScope.withRandomVariables(head.arguments, ruleRandomVarsMapping)

        val goalAndHeadUnification = randomHeadArgs.unify(randomArgs, context.randomVariableScope)
        if (goalAndHeadUnification != null) {
            val randomQuery = query
                .withRandomVariables(context.randomVariableScope, ruleRandomVarsMapping)
                .substituteVariables(goalAndHeadUnification.variableValues)

            val randomResults = buildLazySequence<Unification>(context.principal) {
                context.fulfillAttach(this, randomQuery, VariableBucket())
            }

            yieldAll(randomResults.mapRemaining { unification ->
                val solutionVars = VariableBucket()

                for (randomGoalVariable in randomArgs.variables)
                {
                    if (goalAndHeadUnification.variableValues.isInstantiated(randomGoalVariable)) {
                        val value = goalAndHeadUnification.variableValues[randomGoalVariable]
                            .substituteVariables(unification.variableValues.asSubstitutionMapper())
                            .substituteVariables(goalAndHeadUnification.variableValues.asSubstitutionMapper())

                        solutionVars.instantiate(randomGoalVariable, value)
                    }
                    else if (unification.variableValues.isInstantiated(randomGoalVariable)) {
                        val originalVar = goalRandomVarsMapping.getOriginal(randomGoalVariable)!!
                        solutionVars.instantiate(originalVar, unification.variableValues[randomGoalVariable])
                    }
                }

                Unification(solutionVars
                    .withVariablesResolvedFrom(goalRandomVarsMapping))
            })
        }
        // else: does not match the rule head
    }

    override fun toString() = "$head :- $query"

    override fun conditionsForBehaviour(inRuntime: PrologRuntimeEnvironment, callingModule: Module, level: DeterminismLevel): List<ConstrainedTerm>? {
        return query.conditionsForBehaviour(inRuntime, callingModule, level, RandomVariableScope())
            ?.map { queryBehaviour ->
                ConstrainedTerm(
                    head.substituteVariables(queryBehaviour.instantiatesOnSuccess.variableValues.asSubstitutionMapper()),
                    queryBehaviour.behaviourExpectedGiven
                )
            }
    }

    private fun Query.conditionsForBehaviour(inRuntime: PrologRuntimeEnvironment, contextModule: Module, level: DeterminismLevel, randomVariableScope: RandomVariableScope): List<QueryBehaviour>? {
        if (level != DeterminismLevel.DETERMINISTIC) return null

        return when (this) {
            is PredicateInvocationQuery -> {
                val (_, invocationTarget) = contextModule.resolveCallable(inRuntime, ClauseIndicator.of(goal)) ?: return null
                if (invocationTarget !is BehaviourExposingPrologCallable) return null
                return (invocationTarget.conditionsForBehaviour(inRuntime, contextModule, level) ?: return null)
                    .mapNotNull { it.translate(goal, randomVariableScope) }
                    .associateWithNotNull { constraint ->
                        // unification tells us the instantiations that occur as the result of the goal.
                        goal.arguments.unify((constraint.structure as CompoundTerm).arguments, randomVariableScope)
                    }
                    .map { (constrainedTerm, unification) ->
                        val constraints = unification.variableValues.values
                            .map { (queryVar, structureValue) ->
                                val constraintFromQueryVar = constrainedTerm.constraints[queryVar] ?: NoopConstraint
                                val constraintFromStructureValue = constrainedTerm.constraints[structureValue as? Variable] ?: NoopConstraint
                                queryVar to constraintFromQueryVar.and(constraintFromStructureValue, randomVariableScope)
                            }
                            .filter { it.second !is NoopConstraint }
                            .toMap()

                        QueryBehaviour(
                            level,
                            constraints,
                            unification
                        )
                    }
            }
            is AndQuery -> {
                var behaviours: List<QueryBehaviour> = listOf(QueryBehaviour(level, emptyMap(), Unification.TRUE))
                for (goal in goals) {
                    val behavioursFlatMapped = ArrayList<QueryBehaviour>(behaviours.size * 2)
                    for (previousBehaviour in behaviours) {
                        val goalBehaviours = goal
                             .substituteVariables(previousBehaviour.instantiatesOnSuccess.variableValues)
                             .conditionsForBehaviour(inRuntime, contextModule, level, randomVariableScope)
                             ?: return null

                        for (goalBehaviour in goalBehaviours) {
                            val combined = previousBehaviour.combineWith(goalBehaviour, randomVariableScope)
                            if (combined != null) {
                                behavioursFlatMapped.add(combined)
                            }
                        }
                    }

                    behaviours = behavioursFlatMapped
                }

                behaviours
            }
            is OrQuery -> if (goals.size == 1) goals.single().conditionsForBehaviour(inRuntime, contextModule, level, randomVariableScope) else {
                TODO()
            }
        }
    }
}

private data class QueryBehaviour(
    val level: DeterminismLevel,
    val behaviourExpectedGiven: Map<Variable, TermConstraint>,
    val instantiatesOnSuccess: Unification
) {
    fun combineWith(other: QueryBehaviour, randomVariableScope: RandomVariableScope): QueryBehaviour? {
        if (level != other.level) return null
        val combinedInstantiations = try {
            instantiatesOnSuccess.combinedWith(other.instantiatesOnSuccess)
        } catch (ex: VariableDiscrepancyException) {
            return null
        }

        val behaviourExpectedGivenCombined = HashMap<Variable, TermConstraint>(instantiatesOnSuccess.variableValues.size + other.instantiatesOnSuccess.variableValues.size)
        behaviourExpectedGivenCombined.putAll(behaviourExpectedGiven)
        for ((otherVariable, otherConstraint) in other.behaviourExpectedGiven) {
            if (otherVariable in behaviourExpectedGivenCombined) {
                val combinedConstraint = behaviourExpectedGivenCombined.getValue(otherVariable).and(otherConstraint, randomVariableScope)
                if (combinedConstraint is ImpossibleConstraint) {
                    return null
                }
                behaviourExpectedGivenCombined[otherVariable] = combinedConstraint
            }
        }

        return QueryBehaviour(level, behaviourExpectedGivenCombined, combinedInstantiations)
    }
}