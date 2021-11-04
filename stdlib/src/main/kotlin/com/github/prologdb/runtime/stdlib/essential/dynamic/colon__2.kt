package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinColon2 = nativeRule(":", 2) { args, ctxt ->
    val moduleName = args[0] as? Atom
        ?: throw PrologRuntimeException("Argument 0 to :/2 must be an atom, got ${args[0].prologTypeName}")
    val goal = args[1] as? CompoundTerm
        ?: throw PrologRuntimeException("Argument 1 to :/2 must be a compound term, got ${args[1].prologTypeName}")

    val module = ctxt.rootAvailableModules[moduleName.name]
        ?: throw PrologRuntimeException("Module $moduleName is not loaded")

    return@nativeRule module.deriveScopedProofSearchContext(ctxt)
        .fulfillAttach(this, compoundToQuery(goal), VariableBucket())
}
