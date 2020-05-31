package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import unify
import variables
import withRandomVariables

open class Rule(val head: CompoundTerm, val query: Query) : Clause, PrologCallable {
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
    override val fulfill: PrologCallableFulfill = { arguments, context ->
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

            yieldAllFinal(randomResults.mapRemaining { unification ->
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
        else null
    }

    override fun toString() = "$head :- $query"

    var sourceInformation: PrologSourceInformation = NullSourceInformation
}
