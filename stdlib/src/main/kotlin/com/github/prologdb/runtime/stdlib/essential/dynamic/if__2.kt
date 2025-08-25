package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.remainingToList
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.unification.Unification

val BuiltinIf2 = nativeRule("if", 2) { args, ctx ->
    val condition = args.getQuery(0)
    val then = args.getQuery(1)

    val conditionResults = buildLazySequence(principal) {
        ctx.fulfillAttach(this, condition, Unification.TRUE)
    }
    val conditionFirstResult = await(conditionResults.limitRemaining(1).remainingToList()).firstOrNull()
    return@nativeRule if (conditionFirstResult != null) {
        ctx.fulfillAttach(this, then, conditionFirstResult)
    } else {
        Unification.TRUE
    }
}