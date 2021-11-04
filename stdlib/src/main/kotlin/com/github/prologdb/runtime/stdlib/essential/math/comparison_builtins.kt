package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinGreaterThan2 = nativeRule(">", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber > args[1].asPrologNumber)
}

internal val BuiltinGreaterThanOrEqual2 = nativeRule(">=", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber >= args[1].asPrologNumber)
}

internal val BuiltinLessThan2 = nativeRule("<", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber < args[1].asPrologNumber)
}

internal val BuiltinLessThanOrEqual2 = nativeRule("=<", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber <= args[1].asPrologNumber)
}
