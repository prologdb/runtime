package com.github.prologdb.runtime.builtin

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.OperatorDefinition
import com.github.prologdb.runtime.knowledge.library.OperatorType
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinNot = nativeRule("not", 1) { args, context ->
    val arg0 = args[0] as? CompoundTerm ?: return@nativeRule

    val proofSequence = buildLazySequence<Unification>(context.principal) {
        context.fulfillAttach(this, PredicateInvocationQuery(arg0), VariableBucket())
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
    add(CompoundTerm("=", arrayOf(X, X)))

    add(BuiltinNot)

    // \+/1
    add(Rule(
        CompoundTerm("\\+", arrayOf(A)),
        PredicateInvocationQuery(
            CompoundTerm("not", arrayOf(A))
        )
    ))

    // \=(A, B) :- not(=(A, B)).
    add(Rule(
        CompoundTerm("\\=", arrayOf(A, B)),
        PredicateInvocationQuery(
            CompoundTerm("not", arrayOf(
                CompoundTerm("=", arrayOf(A, B))
            ))
        )
    ))

    add(BuiltinIdentity)

    add(Rule(
        CompoundTerm("\\==", arrayOf(A, B)),
        PredicateInvocationQuery(
            CompoundTerm("not", arrayOf(
                CompoundTerm("==", arrayOf(A, B))
            ))
        )
    ))

    defineOperator(OperatorDefinition(900, OperatorType.FY, "\\+"))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "="))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "=="))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\="))
    defineOperator(OperatorDefinition(700, OperatorType.XFX, "\\=="))
}