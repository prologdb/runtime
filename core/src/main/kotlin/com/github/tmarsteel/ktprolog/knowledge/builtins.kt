package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.query.PredicateQuery
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable
import com.github.tmarsteel.ktprolog.unification.Unification

private val surrogateVarLHS = Variable("LHS")
private val surrogateVarRHS = Variable("RHS")
private val surrogateVarX = Variable("X")

abstract class BuiltinPredicate(name: String, vararg arguments: Term) : Predicate(name, arguments) {
    override val variables: Set<Variable>
        get() = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this
}

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