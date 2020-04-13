package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinGreaterThan = nativeRule(">", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber > args[1].asPrologNumber)
}

internal val BuiltinGreaterThanOrEqual = nativeRule(">=", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber >= args[1].asPrologNumber)
}

internal val BuiltinLessThan = nativeRule("<", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber < args[1].asPrologNumber)
}

internal val BuiltinLessThanOrEqual = nativeRule("=<", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber <= args[1].asPrologNumber)
}
