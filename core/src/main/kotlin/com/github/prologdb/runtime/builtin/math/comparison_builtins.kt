package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinGreaterThan = prologBuiltin(">", 2) { args, _ ->
    if (args[0].asPrologNumber > args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinGreaterThanOrEqual = prologBuiltin(">=", 2) { args, _ ->
    if (args[0].asPrologNumber >= args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinLessThan = prologBuiltin("<", 2) { args, _ ->
    if (args[0].asPrologNumber < args[1].asPrologNumber) yield(Unification.TRUE)
}

internal val BuiltinLessThanOrEqual = prologBuiltin("=<", 2) { args, _ ->
    if (args[0].asPrologNumber <= args[1].asPrologNumber) yield(Unification.TRUE)
}