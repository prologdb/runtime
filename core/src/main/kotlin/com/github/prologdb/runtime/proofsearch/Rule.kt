package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

open class Rule(val head: CompoundTerm, val query: Query) : Clause, PrologCallable {
    override val functor = head.functor
    override val arity = head.arity

    fun prepareCall(arguments: Array<out Term>, context: ProofSearchContext): PreparedCall? {
        val mediator = VariableScopeMediator.tryCreate(arguments, head.arguments, context) ?: return null
        val sharedScopeQuery = mediator.innerToShared(query)
        return PreparedCall(mediator, sharedScopeQuery, context)
    }

    val fulfillPreparedCall: suspend LazySequenceBuilder<Unification>.(PreparedCall) -> Unification? = { preparation ->
        preparation.proofSearchContext.fulfillAttach(this, preparation.sharedQuery, Unification.TRUE)
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
            yieldAllFinal(
                buildLazySequence<Unification>(context.principal) {
                    fulfillPreparedCall.invoke(this, preparation)
                }
                    .mapRemaining {
                        preparation.untranslateResult(it, context.randomVariableScope)
                    }
            )
        }
    }

    override fun toString() = "$head :- $query"

    var sourceInformation: PrologSourceInformation = NullSourceInformation

    inner class PreparedCall internal constructor(
        val mediator: VariableScopeMediator,
        val sharedQuery: Query,
        val proofSearchContext: ProofSearchContext,
    ) {
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
            return mediator.innerToShared(term)
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
        fun untranslateResult(solution: Unification, randomVariableScope: RandomVariableScope): Unification {
            return mediator.sharedToOuter(solution)
        }

        fun untranslateResult(term: CompoundTerm): CompoundTerm {
            return mediator.sharedToOuter(term)
        }

        /**
         * @param tailCall the tail call, as given in the original code.
         * @return The arguments for a subsequent tail-recursive call, provided that's possible (null othwerise)
         */
        fun getTailCallArguments(stackFrameAfterLastGoal: Unification, tailCall: CompoundTerm): Array<out Term>? {
            if (mediator.isUntranslateOperationStateful) {
                return null
            }

            val tailCallConcrete = translateClausePart(tailCall)
                .substituteVariables(stackFrameAfterLastGoal.asSubstitutionMapper())
            return untranslateResult(tailCallConcrete).arguments
        }
    }
}
