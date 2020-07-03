package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import unify
import variables
import withRandomVariables

open class Rule(val head: CompoundTerm, val query: Query) : Clause, PrologCallable {
    override val functor = head.functor
    override val arity = head.arity

    fun prepareCall(arguments: Array<out Term>, context: ProofSearchContext): PreparedCall? {
        val argumentsRandomVarsMapping = VariableMapping()
        val randomArgs = context.randomVariableScope.withRandomVariables(arguments, argumentsRandomVarsMapping)

        val ruleRandomVarsMapping = VariableMapping()
        val randomHeadArgs = context.randomVariableScope.withRandomVariables(head.arguments, ruleRandomVarsMapping)

        val goalAndHeadUnification = randomHeadArgs.unify(randomArgs, context.randomVariableScope)
        return if (goalAndHeadUnification != null) {
            val randomQuery = query
                .withRandomVariables(context.randomVariableScope, ruleRandomVarsMapping)
                .substituteVariables(goalAndHeadUnification.variableValues)

            PreparedCall(
                context,
                randomQuery,
                argumentsRandomVarsMapping,
                randomArgs,
                ruleRandomVarsMapping,
                goalAndHeadUnification
            )
        } else null
    }

    val fulfillPreparedCall: suspend LazySequenceBuilder<Unification>.(PreparedCall) -> Unification? = { preparation ->
        val (context, randomQuery) = preparation
        context.fulfillAttach(this, randomQuery, VariableBucket())
    }

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
        prepareCall(arguments, context)?.let { preparation ->
            yieldAllFinal(buildLazySequence<Unification>(context.principal) {
                fulfillPreparedCall.invoke(this, preparation)
            }.mapRemaining(preparation::derandomize))
        }
    }

    override fun toString() = "$head :- $query"

    var sourceInformation: PrologSourceInformation = NullSourceInformation

    data class PreparedCall internal constructor(
        val context: ProofSearchContext,
        val randomQuery: Query,
        private val argumentsRandomVarsMapping: VariableMapping,
        private val randomArguments: Array<out Term>,
        private val ruleRandomVarsMapping: VariableMapping,
        private val goalAndHeadUnification: Unification
    ) {
        fun randomize(term: CompoundTerm): CompoundTerm {
            return context.randomVariableScope.withRandomVariables(term, ruleRandomVarsMapping)
                .substituteVariables(goalAndHeadUnification.variableValues.asSubstitutionMapper())
        }

        fun derandomize(solution: Unification): Unification {
            val solutionVars = VariableBucket()

            for (randomGoalVariable in randomArguments.variables)
            {
                if (goalAndHeadUnification.variableValues.isInstantiated(randomGoalVariable)) {
                    val value = goalAndHeadUnification.variableValues[randomGoalVariable]
                        .substituteVariables(solution.variableValues.asSubstitutionMapper())
                        .substituteVariables(goalAndHeadUnification.variableValues.asSubstitutionMapper())

                    solutionVars.instantiate(randomGoalVariable, value)
                }
                else if (solution.variableValues.isInstantiated(randomGoalVariable)) {
                    val originalVar = argumentsRandomVarsMapping.getOriginal(randomGoalVariable)!!
                    solutionVars.instantiate(originalVar, solution.variableValues[randomGoalVariable])
                }
            }

            return Unification(solutionVars.withVariablesResolvedFrom(argumentsRandomVarsMapping))
        }
    }
}
