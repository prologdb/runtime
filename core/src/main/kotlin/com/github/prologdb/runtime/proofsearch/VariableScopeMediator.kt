package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.UnificationBuilder

/**
 * Utility to correctly isolate two pieces of code that have distinct variable scopes. That is
 * achieved using random variables from the given [ProofSearchContext]:
 *
 * both inner and outer scope terms will be put into a single variable scope with random variables,
 * thereby keeping a clear separation between the two distinct original scopes. The instance of
 * [VariableScopeMediator] keeps track of which variables in the original scopes correspond to what randomized
 * variables in the shared scope. Then, code from both scopes can be executed in a shared environment
 * and results with variables from the shared scope ([Term]s and [Unification]s) can be then be
 * translated back into either of the two original scopes.
 */
class VariableScopeMediator internal constructor(
    private val proofSearchContext: ProofSearchContext,
    private val outerScopeRandomVarsMapping: VariableMapping,
    private val randomizedOuterScopeVariables: Iterable<Variable>,
    private val innerScopeRandomVarsMapping: VariableMapping,
    private val innerAndOuterRandomizedUnification: Unification,
) {
    /**
     * Adjusts variables in the given term as if that term had been part of the inner scope.
     *
     * Assume `innerScopeTerm` was `=(X, 2)` when [VariableScopeMediator.Companion.tryCreate] was called.
     *
     * The randomization will turn the innerScopeTerm into `=(_G2, 2)`.
     *
     * Then, this method would convert `>(X, 2)` into `>(_G2, 2)`.
     */
    fun innerToShared(term: Term): Term {
        return proofSearchContext.randomVariableScope.withRandomVariables(term, innerScopeRandomVarsMapping)
            .substituteVariables(innerAndOuterRandomizedUnification.asSubstitutionMapper())
    }

    fun innerToShared(term: CompoundTerm): CompoundTerm = innerToShared(term as Term) as CompoundTerm

    fun innerToShared(query: Query): Query {
        return query.withRandomVariables(proofSearchContext.randomVariableScope, innerScopeRandomVarsMapping)
            .substituteVariables(innerAndOuterRandomizedUnification)
    }

    /**
     * Exchanges connected variables from the inner scope to the outer scope.
     *
     * Assume `outerScopeTerm` is `p(A)` and `innerScopeTerm` is `p(X)`.
     *
     * Randomization will turn these into `p(_G1)` and `p(_G2)` respectively,
     * but associates `A` in the outer scope to `X` in the inner scope.
     *
     * In this example, this method would translate this unification
     *
     * ```
     * _G2 = 2,
     * Foo = 0
     * ```
     *
     * into
     *
     * ```
     * A = 2
     * ```
     *
     * thereby also dropping variables unknown to the outer scope (as per `outerScopeTerm`).
     */
    fun sharedToOuter(solution: Unification): Unification {
        val solutionVars = UnificationBuilder()

        for (randomGoalVariable in randomizedOuterScopeVariables) {
            if (innerAndOuterRandomizedUnification.isInstantiated(randomGoalVariable)) {
                val value = innerAndOuterRandomizedUnification[randomGoalVariable]
                    .substituteVariables(solution.asSubstitutionMapper())
                    .substituteVariables(innerAndOuterRandomizedUnification.asSubstitutionMapper())

                solutionVars.instantiate(randomGoalVariable, value, proofSearchContext.randomVariableScope)
            }
            else if (solution.isInstantiated(randomGoalVariable)) {
                outerScopeRandomVarsMapping.getOriginal(randomGoalVariable)?.let { originalVar ->
                    solutionVars.instantiate(originalVar, solution[randomGoalVariable], proofSearchContext.randomVariableScope)
                }
            }
        }

        return solutionVars.build().withVariablesResolvedFrom(outerScopeRandomVarsMapping, proofSearchContext.randomVariableScope)
    }

    fun sharedToOuter(term: Term): Term {
        return term.substituteVariables {
            outerScopeRandomVarsMapping.getOriginal(it)
                ?: innerScopeRandomVarsMapping.getOriginal(it)
                ?: it
        }
    }

    fun sharedToOuter(term: CompoundTerm): CompoundTerm = sharedToOuter(term as Term) as CompoundTerm

    fun innerToOuter(term: Term): Term {
        return term.let(this::innerToShared).let(this::sharedToOuter)
    }

    fun outerToShared(term: Term): Term {
        return proofSearchContext.randomVariableScope.withRandomVariables(term, outerScopeRandomVarsMapping)
            .substituteVariables(innerAndOuterRandomizedUnification.asSubstitutionMapper())
    }

    fun sharedToInner(term: Term): Term {
        return term.substituteVariables {
            innerScopeRandomVarsMapping.getOriginal(it)
                ?: outerScopeRandomVarsMapping.getOriginal(it)
                ?: it
        }
    }

    fun outerToInner(term: Term): Term = term.let(this::outerToShared).let(this::sharedToInner)

    /**
     * Whether the return values [sharedToOuter] vary depending on the variables in the input. This information
     * is key in deciding where tail-call optimization can be utilized.
     */
    val isUntranslateOperationStateful: Boolean by lazy {
        return@lazy randomizedOuterScopeVariables.any { headVar ->
            if (!innerAndOuterRandomizedUnification.isInstantiated(headVar)) {
                return@any false
            }
            val value = innerAndOuterRandomizedUnification[headVar]
            return@any value !is Variable && !value.isGround
        }
    }

    companion object {
        /**
         * [outerScopeTerm] and [innerScopeTerm] must unify, returns `null` otherwise.
         * @param outerScopeTerm a term containing all variables from the outer scope that must not collide with the inner scope
         * @param innerScopeTerm a term containing all variables from the inner scope that will need to be translated back to the outer scope
         * @return a [VariableScopeMediator] that can translate the variables mentioned in [outerScopeTerm] and [innerScopeTerm]
         * between the two scopes. Associates those variables from both scopes that get instantiated to each other by unification of the
         * two terms.
         */
        fun tryCreate(outerScopeTerm: Term, innerScopeTerm: Term, context: ProofSearchContext): VariableScopeMediator? {
            val outerScopeRandomVarsMapping = VariableMapping()
            val randomizedOuterScopeTerm = context.randomVariableScope.withRandomVariables(outerScopeTerm, outerScopeRandomVarsMapping)

            val innerScopeRandomVarsMapping = VariableMapping()
            val randomizedInnerScopeTerm = context.randomVariableScope.withRandomVariables(innerScopeTerm, innerScopeRandomVarsMapping)

            val innerAndOuterRandomizedUnification = randomizedInnerScopeTerm.unify(randomizedOuterScopeTerm, context.randomVariableScope)
                ?: return null

            return VariableScopeMediator(
                context,
                outerScopeRandomVarsMapping,
                randomizedOuterScopeTerm.variables,
                innerScopeRandomVarsMapping,
                innerAndOuterRandomizedUnification
            )
        }

        /**
         * Lists consisting of the terms in [outerScopeTerms] and [innerScopeTerms] must unify, returns `null` otherwise.
         * @param outerScopeTerms terms containing all variables from the outer scope that must not collide with the inner scope
         * @param innerScopeTerms terms containing all variables from the inner scope that will need to be translated back to the outer scope
         * @return a [VariableScopeMediator] that can translate the variables mentioned in [outerScopeTerms] and [innerScopeTerms]
         * between the two scopes. Associates those variables from both scopes that get instantiated to each other by unification of the
         * two terms.
         */
        fun tryCreate(outerScopeTerms: Array<out Term>, innerScopeTerms: Array<out Term>, context: ProofSearchContext): VariableScopeMediator? {
            val outerScopeRandomVarsMapping = VariableMapping()
            val randomizedOuterScopeTerms = context.randomVariableScope.withRandomVariables(outerScopeTerms, outerScopeRandomVarsMapping)

            val innerScopeRandomVarsMapping = VariableMapping()
            val randomizedInnerScopeTerms = context.randomVariableScope.withRandomVariables(innerScopeTerms, innerScopeRandomVarsMapping)

            val innerAndOuterRandomizedUnification = randomizedInnerScopeTerms.unify(randomizedOuterScopeTerms, context.randomVariableScope)
                ?: return null

            return VariableScopeMediator(
                context,
                outerScopeRandomVarsMapping,
                randomizedOuterScopeTerms.variables,
                innerScopeRandomVarsMapping,
                innerAndOuterRandomizedUnification
            )
        }
    }
}