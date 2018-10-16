package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.async.LazySequence
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.unification.Unification

/** Implements the builtin @</2 */
val BuiltinTermLessThan = prologBuiltin("@<", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(args[0] < args[1])
    )
}

/** Implements the builtin @=</2 */
val BuiltinTermLessThanOrEqual = prologBuiltin("@=<", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(args[0] <= args[1])
    )
}

/** Implements the builtin @>/2 */
val BuiltinTermGreaterThan = prologBuiltin("@>", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(args[0] > args[1])
    )
}

/** Implements the builtin @>=/2 */
val BuiltinTermGreaterThanOrEqual = prologBuiltin("@>=", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(args[0] >= args[1])
    )
}

private val AtomLessThan = Atom("<")
private val AtomGreaterThan = Atom("<")
private val AtomEqual = Atom("=")
val BuiltinCompare = prologBuiltin("compare", 3) { args, _, _ ->
    val inputForOrder = args[0]

    val actualOrder = if (args[0] > args[1]) {
        AtomGreaterThan
    } else if (args[0] < args[1]) {
        AtomLessThan
    } else {
        AtomEqual
    }

    return@prologBuiltin LazySequence.ofNullable(inputForOrder.unify(actualOrder))
}

/**
 * Defines predicates for the standard order of terms.
 */
val ComparisonLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(BuiltinTermLessThan)
        add(BuiltinTermLessThanOrEqual)
        add(BuiltinTermGreaterThan)
        add(BuiltinTermGreaterThanOrEqual)
        add(BuiltinCompare)

        defineOperator(OperatorDefinition(700, OperatorType.XFX, "@<"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "@=<"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "@>"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "@>="))
    }
}