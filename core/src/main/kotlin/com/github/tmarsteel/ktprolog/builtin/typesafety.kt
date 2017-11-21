package com.github.tmarsteel.ktprolog.builtin

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.knowledge.library.DefaultOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.DoublyIndexedLibraryEntryStore
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.knowledge.library.SimpleLibrary
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.unification.Unification

val TypeSafetyLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(IsAtomPredicate)
    }
}

object IsAtomPredicate : BuiltinPredicate("atom", X) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val surrogateUnification = super.unify(rhs, randomVarsScope) ?: return null

        if (surrogateUnification.variableValues[X] is Atom) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}