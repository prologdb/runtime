package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable

val BuiltinTermVariables2 = nativeRule("term_variables", 2) { args, ctxt ->
    val target = args[1]
    if (target !is Variable && target !is PrologList) {
        throw ArgumentTypeError(1, target, Variable::class.java, PrologList::class.java)
    }

    return@nativeRule PrologList(args[0].variables.toList()).unify(target, ctxt.randomVariableScope)
}
