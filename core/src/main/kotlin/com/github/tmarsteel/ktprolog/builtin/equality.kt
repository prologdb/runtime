package com.github.tmarsteel.ktprolog.builtin

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.knowledge.KnowledgeBase
import com.github.tmarsteel.ktprolog.knowledge.Rule
import com.github.tmarsteel.ktprolog.knowledge.library.DefaultOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorDefinition
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorType
import com.github.tmarsteel.ktprolog.query.PredicateQuery
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.unification.Unification

/**
 * Defines the ISO equality and inequality predicates and operators.
 */
object EqualityLibrary : Library {
    private val operators = DefaultOperatorRegistry(false)
    init {
        operators.defineOperator(OperatorDefinition(700, OperatorType.XFX, "="))
        operators.defineOperator(OperatorDefinition(700, OperatorType.XFX, "=="))
        operators.defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\="))
        operators.defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\=="))
    }

    override val exports = listOf(
        // =(X, X)
        Predicate("=", arrayOf(X, X)),

        NegationRule,

        // \=(A, B) :- not(=(A, B)).
        Rule(
            Predicate("\\=", arrayOf(A, B)),
            PredicateQuery(
                Predicate("not", arrayOf(
                    Predicate("=", arrayOf(A, B))
                ))
            )
        ),

        IdentityPredicate,

        Rule(
            Predicate("\\==", arrayOf(A, B)),
            PredicateQuery(
                Predicate("not", arrayOf(
                    Predicate("==", arrayOf(A, B))
                ))
            )
        )
    )

    override fun getPrefixDefinition(name: String): OperatorDefinition? = operators.getPrefixDefinition(name)

    override fun getInfixDefinition(name: String): OperatorDefinition? = operators.getInfixDefinition(name)

    override fun getPostfixDefinition(name: String): OperatorDefinition? = operators.getPostfixDefinition(name)

    override val allOperators: Iterable<OperatorDefinition>
        get() = operators.allOperators
}

object NegationRule : Rule(Predicate("not", arrayOf(X)), PredicateQuery(Predicate("not", arrayOf(X)))) {
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