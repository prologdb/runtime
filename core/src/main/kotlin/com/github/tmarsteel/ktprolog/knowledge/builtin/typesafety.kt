package com.github.tmarsteel.ktprolog.knowledge.builtin

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.unification.Unification

object IsAtomPredicate : BuiltinPredicate("atom", surrogateVarX) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val surrogateUnification = super.unify(rhs, randomVarsScope) ?: return null

        if (surrogateUnification.variableValues[surrogateVarX] is Atom) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}