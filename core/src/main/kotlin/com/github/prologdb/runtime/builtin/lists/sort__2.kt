package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

internal val SortBuiltin = nativeRule("sort", 2) { args, context ->
    val inputUnsorted = args[0]
    val inputSorted = args[1]

    if (inputUnsorted is Variable) {
        throw PrologRuntimeException("Arguments not sufficiently instantiated: argument 1 to sort/2")
    }

    if (inputUnsorted !is PrologList) {
        throw PrologRuntimeException("Type error: argument 1 to sort/2 must be a list, got $inputUnsorted")
    }

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
