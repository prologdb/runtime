package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.async.LazySequence
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

val BuiltinNot = prologBuiltin("not", 1) { args, knowledgeBase, randomVarsScope ->
    val arg0 = args[0] as? Predicate ?: return@prologBuiltin Unification.NONE

    val proofSequence = knowledgeBase.fulfill(arg0, randomVarsScope)
    val hasProof = proofSequence.tryAdvance() != null
    proofSequence.close()

    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(
            !hasProof // this is the core logic here
        )
    )
}

val BuiltinIdentity = prologBuiltin("==", 2) { args, _, _ ->
    return@prologBuiltin LazySequence.ofNullable(
        Unification.whether(
            args[0] == args[1]
        )
    )
}

/**
 * Defines the ISO equality and inequality predicates and operators.
 */
val EqualityLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
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
}