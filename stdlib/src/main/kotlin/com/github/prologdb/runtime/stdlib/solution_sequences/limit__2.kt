package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinLimit2 = nativeRule("limit", 2) { args, ctxt ->
    val limit = args.getIntegerInRange(0, 0..Long.MAX_VALUE)
    val goal = args.getQuery(1)

    val solutions = buildLazySequence(ctxt.principal) {
        ctxt.fulfillAttach(this, goal, VariableBucket())
    }

    yieldAllFinal(solutions.limitRemaining(limit))
}