package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm

val BuiltinQualifyCallable3 = nativeRule("qualify_callable", 3) { args, ctxt ->
    val callableTerm = args[0]
    val defaultModule = args.getTyped<Atom>(1)
    val output = args[2]

    if (callableTerm is CompoundTerm && callableTerm.functor == ":" && callableTerm.arity == 2) {
        return@nativeRule output.unify(callableTerm, ctxt.randomVariableScope)
    }

    return@nativeRule output.unify(CompoundTerm(":", arrayOf(defaultModule, callableTerm)), ctxt.randomVariableScope)
}