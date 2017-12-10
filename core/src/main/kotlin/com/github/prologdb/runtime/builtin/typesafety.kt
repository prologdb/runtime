package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedLibraryEntryStore
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.SimpleLibrary
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

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