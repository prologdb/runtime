package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.unification.Unification

/** Implements the builtin @</2 */
val BuiltinTermLessThan = nativeRule("@<", 2) { args, _ ->
    Unification.whether(args[0] < args[1])
}

/** Implements the builtin @=</2 */
val BuiltinTermLessThanOrEqual = nativeRule("@=<", 2) { args, _ ->
    Unification.whether(args[0] <= args[1])
}

/** Implements the builtin @>/2 */
val BuiltinTermGreaterThan = nativeRule("@>", 2) { args, _ ->
    Unification.whether(args[0] > args[1])
}

/** Implements the builtin @>=/2 */
val BuiltinTermGreaterThanOrEqual = nativeRule("@>=", 2) { args, _ ->
    Unification.whether(args[0] >= args[1])
}

private val AtomLessThan = Atom("<")
private val AtomGreaterThan = Atom(">")
private val AtomEqual = Atom("=")
val BuiltinCompare = nativeRule("compare", 3) { args, ctxt ->
    val inputForOrder = args[0]

    val actualOrder = when {
        args[1] > args[2] -> AtomGreaterThan
        args[1] < args[2] -> AtomLessThan
        else              -> AtomEqual
    }

    return@nativeRule inputForOrder.unify(actualOrder, ctxt.randomVariableScope)
}

/**
 * Defines predicates for the standard order of terms.
 */
val EssentialComparisonModule = nativeModule("\$comparison") {
    add(BuiltinTermLessThan)
    add(BuiltinTermLessThanOrEqual)
    add(BuiltinTermGreaterThan)
    add(BuiltinTermGreaterThanOrEqual)
    add(BuiltinCompare)
}
