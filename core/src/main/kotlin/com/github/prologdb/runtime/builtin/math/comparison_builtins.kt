package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.async.LazySequence
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinGreaterThan = prologBuiltin(">", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(
            args[0].asPrologNumber > args[1].asPrologNumber
        )
    )
}

internal val BuiltinGreaterThanOrEqual = prologBuiltin(">=", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(
            args[0].asPrologNumber >= args[1].asPrologNumber
        )
    )
}

internal val BuiltinLessThan = prologBuiltin("<", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(
            args[0].asPrologNumber < args[1].asPrologNumber
        )
    )
}

internal val BuiltinLessThanOrEqual = prologBuiltin("=<", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(
            args[0].asPrologNumber <= args[1].asPrologNumber
        )
    )
}