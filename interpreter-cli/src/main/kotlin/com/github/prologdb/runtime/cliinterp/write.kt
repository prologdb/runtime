package com.github.prologdb.runtime.cliinterp

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.unification.Unification

val BuiltinWrite1 = nativeRule("write", 1) { args, ctx ->
    println(args.get(0).toStringUsingOperatorNotations(ctx.operators))
    Unification.TRUE
}