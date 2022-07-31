package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term

val BuiltinSort2 = nativeRule("sort", 2) { args, context ->
    val inputUnsorted = args.getTyped<PrologList>(0)
    val inputSorted = args[1]

    val inputElementsSorted = inputUnsorted.elements.sorted()

    val inputElementsSortedUnique = mutableListOf<Term>()
    inputElementsSorted.forEach { notUniqueTerm ->
        if (inputElementsSortedUnique.none { it.compareTo(notUniqueTerm) == 0 }) {
            inputElementsSortedUnique.add(notUniqueTerm)
        }
    }

    val sorted = PrologList(inputElementsSortedUnique, inputUnsorted.tail)
    return@nativeRule sorted.unify(inputSorted, context.randomVariableScope)
}
