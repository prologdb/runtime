package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.unification.Unification

val BuiltinOffset2 = nativeRule("offset", 2) { args, ctxt ->
    val offset = args.getIntegerInRange(0, 0..Long.MAX_VALUE)
    val goal = args.getQuery(1)

    yieldAllFinal(
        buildLazySequence(ctxt.principal) {
            ctxt.fulfillAttach(this, goal, Unification.TRUE)
        }
            .skipRemaining(offset)
    )
}