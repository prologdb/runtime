package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term

val BuiltinQualifyCallable3 = nativeRule("qualify_callable", 3) { args, ctxt ->
    val callableTerm = args[0]
    val defaultModule = args.getTyped<Atom>(1)
    val output = args[2]

    return@nativeRule output.unify(qualifyCallable(callableTerm, defaultModule), ctxt.randomVariableScope)
}

fun qualifyCallable(callableTerm: Term, defaultModule: Atom): Term {
    if (callableTerm is CompoundTerm && callableTerm.functor == ":" && callableTerm.arity == 2) {
        return callableTerm
    }

    return CompoundTerm(":", arrayOf(defaultModule, callableTerm))
}