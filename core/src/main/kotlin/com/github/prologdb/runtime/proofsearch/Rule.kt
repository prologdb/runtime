package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

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

            this.PreparedCall(
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
            }.mapRemaining(preparation::untranslateResult))
        }
    }

    override fun toString() = "$head :- $query"

    var sourceInformation: PrologSourceInformation = NullSourceInformation

    inner class PreparedCall internal constructor(
        val context: ProofSearchContext,
        val randomQuery: Query,
        private val argumentsRandomVarsMapping: VariableMapping,
        private val randomArguments: Array<out Term>,
        private val ruleRandomVarsMapping: VariableMapping,
        private val goalAndHeadUnification: Unification
    ) {
        operator fun component1() = context
        operator fun component2() = randomQuery

        /**
         * Adjusts variables in the given term as if that term had been part of the clause body when the call
         * was prepared. For example:
         *
         * ```prolog
         * p(X) :- X = 2.
         * ?- p(A).
         * ```
         *
         * The call preparation will translate
         * * the call into `p(_G1)`
         * * and the clause of `p/1` into `p(_G2) :- _G2 = 2`
         * to avoid variable name collisions with the invocation.
         *
         * In the above example, this method would convert `>(X, 2)` into `>(_G2, 2)`.
         */
        fun translateClausePart(term: CompoundTerm): CompoundTerm {
            return context.randomVariableScope.withRandomVariables(term, ruleRandomVarsMapping)
                .substituteVariables(goalAndHeadUnification.variableValues.asSubstitutionMapper())
        }

        /**
         * Translates the stack frame (invocation-local variables) back to the variable space/scope
         * of the caller. For example:
         *
         * ```prolog
         * p(X) :- X = 2, Local = 3.
         * ?- p(A).
         * ```
         *
         * The call preparation will translate the clause into
         *
         * ```
         * P(_G2) :- _G2 = 2, _G3 = 3.
         * ```
         *
         * After the last clause, the stack frame will look like so:
         *
         * ```
         * _G2 = 2,
         * _G3 = 3
         * ```
         *
         * In this example, this method would translate that stack state into
         *
         * ```
         * A = 2
         * ```
         *
         * thereby also dropping the clause-local variables.
         */
        fun untranslateResult(solution: VariableBucket): VariableBucket {
            val solutionVars = VariableBucket()

            for (randomGoalVariable in randomArguments.variables)
            {
                if (goalAndHeadUnification.variableValues.isInstantiated(randomGoalVariable)) {
                    val value = goalAndHeadUnification.variableValues[randomGoalVariable]
                        .substituteVariables(solution.asSubstitutionMapper())
                        .substituteVariables(goalAndHeadUnification.variableValues.asSubstitutionMapper())

                    solutionVars.instantiate(randomGoalVariable, value)
                }
                else if (solution.isInstantiated(randomGoalVariable)) {
                    argumentsRandomVarsMapping.getOriginal(randomGoalVariable)?.let { originalVar ->
                        solutionVars.instantiate(originalVar, solution[randomGoalVariable])
                    }
                }
            }

            return solutionVars.withVariablesResolvedFrom(argumentsRandomVarsMapping)
        }

        fun untranslateResult(solution: Unification): Unification {
            return Unification(untranslateResult(solution.variableValues))
        }

        fun untranslateResult(term: CompoundTerm): CompoundTerm? {
            return term.substituteVariables {
                argumentsRandomVarsMapping.getOriginal(it) ?: ruleRandomVarsMapping.getOriginal(it) ?: it
            }
        }

        /**
         * @param tailCall the tail call, as given in the original code.
         */
        fun getTailCallArguments(stackFrameAfterLastGoal: Unification, tailCall: CompoundTerm): Array<out Term>? {
            val stepsForHeadUnificationRequired = randomArguments.flatMap { it.variables }.any { headVar ->
                if (goalAndHeadUnification.variableValues.isInstantiated(headVar)) {
                    val value = goalAndHeadUnification.variableValues[headVar]
                    value !is Variable && value.variables.isNotEmpty()
                } else {
                    false
                }
            }

            if (stepsForHeadUnificationRequired) {
                return null
            }

            val tailCallConcrete = translateClausePart(tailCall)
                .substituteVariables(stackFrameAfterLastGoal.variableValues.asSubstitutionMapper())
            return untranslateResult(tailCallConcrete)?.arguments
        }
    }
}
