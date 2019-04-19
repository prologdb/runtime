package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.builtin.nativePredicate
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinGreaterThan = nativePredicate(">", 2) { args, _ ->
    if (args[0].asPrologNumber > args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinGreaterThanOrEqual = nativePredicate(">=", 2) { args, _ ->
    if (args[0].asPrologNumber >= args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinLessThan = nativePredicate("<", 2) { args, _ ->
    if (args[0].asPrologNumber < args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinLessThanOrEqual = nativePredicate("=<", 2) { args, _ ->
    if (args[0].asPrologNumber <= args[1].asPrologNumber) yield(Unification.TRUE)
}
