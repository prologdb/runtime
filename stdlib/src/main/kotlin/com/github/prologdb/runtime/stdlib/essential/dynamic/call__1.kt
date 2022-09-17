package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.unification.Unification

val BuiltinCall1 = nativeRule("call", 1) { args, ctxt ->
    val goal = args.getQuery(0)

    ctxt.fulfillAttach(
        this,
        goal,
        Unification.TRUE
    )
}
