package com.github.prologdb.runtime.builtin.typesafety

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.builtin.X
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification

val TypeSafetyLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {

    init {
        // all of these are /1 tests
        add(typeCheckBuiltin("atom") { it is Atom })

        add(typeCheckBuiltin("integer") { it is PrologInteger })
        add(typeCheckBuiltin("decimal") { it is PrologDecimal })
        add(typeCheckBuiltin("number") { it is PrologNumber })

        add(typeCheckBuiltin("string") { it is PrologString })

        add(typeCheckBuiltin("is_list") { it is PrologList })

        add(typeCheckBuiltin("var") { it is Variable })
        add(typeCheckBuiltin("nonvar") { it !is Variable })

        add(typeCheckBuiltin("ground") { it.variables.isEmpty() })
        add(Rule(
            Predicate("nonground", arrayOf(X)),
            PredicateQuery(
                Predicate("not", arrayOf(
                    Predicate("ground", arrayOf(X))
                ))
            )
        ))

        add(TypeofBuiltin)

        defineOperator(OperatorDefinition(900, OperatorType.XFY, "typeof"))
    }
}

/**
 * @return a predicate with the given name and arity 1 that suceeds if the first argument passes
 * the given predicate.
 */
private inline fun typeCheckBuiltin(name: String, crossinline test: (Term) -> Boolean): LibraryEntry {
    return prologBuiltin(name, 1) { args, _, _ ->
        return@prologBuiltin LazySequence.ofNullable(
            Unification.whether(
                test(args[0])
            )
        )
    }
}