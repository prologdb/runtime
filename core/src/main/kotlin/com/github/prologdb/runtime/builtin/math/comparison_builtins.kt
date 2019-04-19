package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinGreaterThan = nativeRule(">", 2) { args, _ ->
    if (args[0].asPrologNumber > args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinGreaterThanOrEqual = nativeRule(">=", 2) { args, _ ->
    if (args[0].asPrologNumber >= args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinLessThan = nativeRule("<", 2) { args, _ ->
    if (args[0].asPrologNumber < args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinLessThanOrEqual = nativeRule("=<", 2) { args, _ ->
    if (args[0].asPrologNumber <= args[1].asPrologNumber) yield(Unification.TRUE)
}
