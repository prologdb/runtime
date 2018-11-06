package com.github.prologdb.runtime.builtin

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinNot = nativeRule("not", 1) { args, context ->
    val arg0 = args[0] as? Predicate ?: return@nativeRule

    val proofSequence = buildLazySequence<Unification>(context.principal) {
        context.fulfillAttach(this, PredicateQuery(arg0), VariableBucket())
    }

    val hasProof = proofSequence.tryAdvance() != null
    proofSequence.close()

    if (!hasProof) yield(Unification.TRUE) // this is the core logic here
}

val BuiltinIdentity = nativeRule("==", 2) { args, _ ->
    if (args[0] == args[1]) yield(Unification.TRUE)
}

/**
 * Defines the ISO equality and inequality predicates and operators.
 */
val EqualityLibrary = nativeLibrary("equality") {
    // =(X, X)
    add(Predicate("=", arrayOf(X, X)))

    add(BuiltinNot)

    // \+/1
    add(Rule(
        Predicate("\\+", arrayOf(A)),
        PredicateQuery(
            Predicate("not", arrayOf(A))
        )
    ))

    // \=(A, B) :- not(=(A, B)).
    add(Rule(
        Predicate("\\=", arrayOf(A, B)),
        PredicateQuery(
            Predicate("not", arrayOf(
                Predicate("=", arrayOf(A, B))
            ))
        )
    ))

    add(BuiltinIdentity)

    add(Rule(
        Predicate("\\==", arrayOf(A, B)),
        PredicateQuery(
            Predicate("not", arrayOf(
                Predicate("==", arrayOf(A, B))
            ))
        )
    ))

    defineOperator(OperatorDefinition(900, OperatorType.FY, "\\+"))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "="))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "=="))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\="))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\=="))
}