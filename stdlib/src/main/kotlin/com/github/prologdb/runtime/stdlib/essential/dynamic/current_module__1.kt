package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.PrologInternalError
import com.github.prologdb.runtime.module.ModuleScopeProofSearchContext
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom

val BuiltinCurrentModule1 = nativeRule("current_module", 1) { args, ctxt ->
    if (ctxt !is ModuleScopeProofSearchContext) {
        throw PrologInternalError("Cannot determine current module: context is not a ${ModuleScopeProofSearchContext::class.qualifiedName}")
    }

    args[0].unify(Atom(ctxt.module.declaration.moduleName), ctxt.randomVariableScope)
}
