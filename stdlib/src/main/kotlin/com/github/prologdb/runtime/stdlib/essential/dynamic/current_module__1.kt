package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom

val BuiltinCurrentModule1 = nativeRule("current_module", 1) { args, ctxt ->
    args[0].unify(Atom(ctxt.module.declaration.moduleName), ctxt.randomVariableScope)
}
