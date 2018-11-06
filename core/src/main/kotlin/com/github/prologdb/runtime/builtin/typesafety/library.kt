package com.github.prologdb.runtime.builtin.typesafety

import com.github.prologdb.runtime.builtin.X
import com.github.prologdb.runtime.builtin.nativeLibrary
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification

val TypeSafetyLibrary = nativeLibrary("typesafety") {
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

/**
 * @return a predicate with the given name and arity 1 that suceeds if the first argument passes
 * the given predicate.
 */
private inline fun typeCheckBuiltin(name: String, crossinline test: (Term) -> Boolean): Clause {
    return nativeRule(name, 1) { args, _ ->
        if (test(args[0])) yield(Unification.TRUE)
    }
}