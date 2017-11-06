package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.query.PredicateQuery
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable
import com.github.tmarsteel.ktprolog.unification.Unification

object NegationRule : Rule(Predicate("not", arrayOf(Variable("X"))), PredicateQuery(Predicate("not", arrayOf(Variable("X"))))) {
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

object IdentityPredicate : Predicate("==", arrayOf(Variable("LHS"), Variable("RHS"))) {
    private val surrogateVarLHS = Variable("LHS")
    private val surrogateVarRHS = Variable("RHS")
    private val surrogatePredicate = Predicate("==", arrayOf(surrogateVarLHS, surrogateVarRHS))

    override val variables: Set<Variable>
        get() = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val surrogateUnification = surrogatePredicate.unify(rhs, randomVarsScope) ?: return null

        if (surrogateUnification.variableValues[surrogateVarLHS] == surrogateUnification.variableValues[surrogateVarRHS]) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}