package com.github.prologdb.runtime.stdlib.essential

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.unification.Unification

/** Implements the builtin @</2 */
val BuiltinTermLessThan2 = nativeRule("@<", 2) { args, _ ->
    Unification.whether(args[0] < args[1])
}

/** Implements the builtin @=</2 */
val BuiltinTermLessThanOrEqual2 = nativeRule("@=<", 2) { args, _ ->
    Unification.whether(args[0] <= args[1])
}

/** Implements the builtin @>/2 */
val BuiltinTermGreaterThan2 = nativeRule("@>", 2) { args, _ ->
    Unification.whether(args[0] > args[1])
}

/** Implements the builtin @>=/2 */
val BuiltinTermGreaterThanOrEqual2 = nativeRule("@>=", 2) { args, _ ->
    Unification.whether(args[0] >= args[1])
}

private val AtomLessThan = Atom("<")
private val AtomGreaterThan = Atom(">")
private val AtomEqual = Atom("=")
val BuiltinCompare3 = nativeRule("compare", 3) { args, ctxt ->
    val inputForOrder = args[0]

    val actualOrder = when {
        args[1] > args[2] -> AtomGreaterThan
        args[1] < args[2] -> AtomLessThan
        else -> AtomEqual
    }

    return@nativeRule inputForOrder.unify(actualOrder, ctxt.randomVariableScope)
}
