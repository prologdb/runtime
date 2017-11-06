package com.github.tmarsteel.ktprolog.builtin

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.knowledge.KnowledgeBase
import com.github.tmarsteel.ktprolog.knowledge.Rule
import com.github.tmarsteel.ktprolog.query.PredicateQuery
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.unification.Unification

object NegationRule : Rule(Predicate("not", arrayOf(surrogateVarX)), PredicateQuery(Predicate("not", arrayOf(surrogateVarX)))) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): Sequence<Unification> {
        if (predicate.name != "not" || predicate.arguments.size != 1) return Unification.NONE
        val arg0 = predicate.arguments[0] as? Predicate ?: return Unification.NONE

        val proof = kb.fulfill(arg0, randomVariableScope)

        if (proof.any()) {
            return Unification.NONE
        } else {
            return sequenceOf(Unification.TRUE)
        }
    }
}

object IdentityPredicate : BuiltinPredicate("==", surrogateVarLHS, surrogateVarRHS) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val surrogateUnification = super.unify(rhs, randomVarsScope) ?: return null

        if (surrogateUnification.variableValues[surrogateVarLHS] == surrogateUnification.variableValues[surrogateVarRHS]) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}