package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

/**
 * Defines the ISO equality and inequality predicates and operators.
 */
val EqualityLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        // =(X, X)
        add(Predicate("=", arrayOf(X, X)))

        add(NegationRule)

        // \=(A, B) :- not(=(A, B)).
        add(Rule(
            Predicate("\\=", arrayOf(A, B)),
            PredicateQuery(
                Predicate("not", arrayOf(
                    Predicate("=", arrayOf(A, B))
                ))
            )
        ))

        add(IdentityPredicate)

        add(Rule(
            Predicate("\\==", arrayOf(A, B)),
            PredicateQuery(
                Predicate("not", arrayOf(
                    Predicate("==", arrayOf(A, B))
                ))
            )
        ))

        defineOperator(OperatorDefinition(700, OperatorType.XFX, "="))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "=="))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\="))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\=="))
    }
}

/**
 * TODO: move to another library because this is technically not a part of equality
 */
object NegationRule : Rule(Predicate("not", arrayOf(X)), PredicateQuery(Predicate("not", arrayOf(X)))) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        if (predicate.name != "not" || predicate.arguments.size != 1) return Unification.NONE
        val arg0 = predicate.arguments[0] as? Predicate ?: return Unification.NONE

        val proofSequence = kb.fulfill(arg0, randomVariableScope)
        val hasProof = proofSequence.tryAdvance() != null
        proofSequence.close()

        if (hasProof) {
            return Unification.NONE
        } else {
            return Unification.SINGLETON
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