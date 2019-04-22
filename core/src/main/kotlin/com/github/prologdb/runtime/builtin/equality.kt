package com.github.prologdb.runtime.builtin

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.ASTModule
import com.github.prologdb.runtime.knowledge.library.ClauseIndicator
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

private val equalityClauses = listOf(
    // =(X, X)
    CompoundTerm("=", arrayOf(X, X)),

    BuiltinNot,

    // \+/1
    Rule(
        CompoundTerm("\\+", arrayOf(A)),
        PredicateInvocationQuery(
            CompoundTerm("not", arrayOf(A))
        )
    ),

    // \=(A, B) :- not(=(A, B)).
    Rule(
        CompoundTerm("\\=", arrayOf(A, B)),
        PredicateInvocationQuery(
            CompoundTerm("not", arrayOf(
                CompoundTerm("=", arrayOf(A, B))
            ))
        )
    ),

    BuiltinIdentity,

    Rule(
        CompoundTerm("\\==", arrayOf(A, B)),
        PredicateInvocationQuery(
            CompoundTerm("not", arrayOf(
                CompoundTerm("==", arrayOf(A, B))
            ))
        )
    )
)

/**
 * Defines the ISO equality and inequality predicates and operators.
 */
val EqualityModule = ASTModule(
    "equality",
    emptyList(),
    equalityClauses,
    emptySet(),
    equalityClauses.map { ClauseIndicator.of(it) }.toSet()
)
