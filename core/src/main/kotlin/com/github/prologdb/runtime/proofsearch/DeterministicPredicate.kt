package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.DeterministicLazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologInvocationContractViolationException

class DeterministicDynamicPrologPredicate(val base: DynamicPrologPredicate) : DynamicPrologPredicate by base {
    override val fulfill: PrologCallableFulfill = { args, ctxt ->
        val onUnfinished: () -> Nothing = {
            val baseIndicator = ClauseIndicator.of(base)
            throw PrologInvocationContractViolationException("Predicate $baseIndicator is declared as deterministic but succeeded with a choicepoint.")
        }
        val onMultiple = onUnfinished
        val onNoResults: () -> Nothing = {
            val baseIndicator = ClauseIndicator.of(base)
            throw PrologInvocationContractViolationException("Predicate $baseIndicator is declared as deterministic but failed without a solution.")
        }

        yieldAllFinal(DeterministicLazySequence(
            buildLazySequence(ctxt.principal) {
                base.fulfill(this, args, ctxt)
            },
            onUnfinished,
            onMultiple,
            onNoResults,
        ))
    }
}