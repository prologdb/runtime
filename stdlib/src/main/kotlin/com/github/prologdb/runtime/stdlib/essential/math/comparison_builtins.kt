package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.unification.Unification

val BuiltinGreaterThan2 = nativeRule(">", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber > args[1].asPrologNumber)
}

val BuiltinGreaterThanOrEqual2 = nativeRule(">=", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber >= args[1].asPrologNumber)
}

val BuiltinLessThan2 = nativeRule("<", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber < args[1].asPrologNumber)
}

val BuiltinLessThanOrEqual2 = nativeRule("=<", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber <= args[1].asPrologNumber)
}

val BuiltinNumericNotEqual2 = nativeRule("=\\=", 2) { args, _ ->
    Unification.whether(args[0].asPrologNumber != args[1].asPrologNumber)
}