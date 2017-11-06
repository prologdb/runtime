package com.github.tmarsteel.ktprolog.builtin

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.unification.Unification

object TypeSafetyLibrary : Library {
    override val exports = listOf(
        IsAtomPredicate
    )
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