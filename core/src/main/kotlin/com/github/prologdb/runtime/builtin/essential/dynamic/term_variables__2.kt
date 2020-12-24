package com.github.prologdb.runtime.builtin.essential.dynamic

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable

val BuiltinTermVariables2 = nativeRule("term_variables", 2) { args, ctxt ->
    val target = args[1]
    if (target !is Variable && target !is PrologList) {
        throw PrologRuntimeException("Argument 2 to term_variables/2 must be unbound or a list, got " + target.prologTypeName)
    }

    return@nativeRule PrologList(args[0].variables.toList()).unify(target, ctxt.randomVariableScope)
}
