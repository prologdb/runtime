package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinColon2 = nativeRule(":", 2) { args, ctxt ->
    val moduleName = args.getTyped<Atom>(0)
    val goal = args.getTyped<CompoundTerm>(1)

    val module = ctxt.rootAvailableModules[moduleName.name]
        ?: throw PrologRuntimeException("Module $moduleName is not loaded")

    return@nativeRule module.deriveScopedProofSearchContext(ctxt)
        .fulfillAttach(this, compoundToQuery(goal), VariableBucket())
}
