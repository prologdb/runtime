package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.analyzation.constraint.ConstrainedTerm
import com.github.prologdb.runtime.analyzation.constraint.DeterminismLevel
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
        TODO()
    }

    private fun Query.conditionsForBehaviour(inRuntime: PrologRuntimeEnvironment, contextModule: Module, level: DeterminismLevel): List<Map<Variable, TermConstraint>>? {
        return when (query) {
            is PredicateInvocationQuery -> {
                val unificationConstraints = query.goal.conditionsForBehaviour(inRuntime, contextModule, level)
                val invocationTarget = contextModule.resolveCallable(inRuntime, ClauseIndicator.of(query.goal))
                TODO()
            }
            is AndQuery -> if (query.goals.size == 1) query.goals.single().conditionsForBehaviour(inRuntime, contextModule, level) else {
                TODO()
            }
            is OrQuery -> if (query.goals.size == 1) query.goals.single().conditionsForBehaviour(inRuntime, contextModule, level) else {
                TODO()
            }
            else -> null
        }
    }
}