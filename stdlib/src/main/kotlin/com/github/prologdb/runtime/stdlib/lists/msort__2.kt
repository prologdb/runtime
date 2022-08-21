package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList

val BuiltinMSort2 = nativeRule("msort", 2) { args, ctxt ->
    val inputUnsorted = args.getTyped<PrologList>(0)
    val inputSorted = args.get(1)

    if (inputUnsorted.tail != null) {
        throw PrologInvocationContractViolationException("List-to-sort cannot have a tail")
    }

    val sorted = PrologList(inputUnsorted.elements.sorted())
    return@nativeRule inputSorted.unify(sorted, ctxt.randomVariableScope)
}