package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.Clause
import com.github.prologdb.runtime.knowledge.library.OperatorDefinition
import com.github.prologdb.runtime.knowledge.library.OperatorType
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification

val TypeofBuiltin = nativeRule("typeof", 2) { args, _ ->
    val arg0 = args[0]
    val arg1 = args[1]

    if (arg1 is Variable) {
        val actualValueArg1 = Atom(arg0.prologTypeName)
        yield(arg1.unify(actualValueArg1))
    } else {
        if (arg1 !is Atom) throw PrologRuntimeException("Type error: argument 2 to typeof/2 must be an atom or unbound")

        // typecheck; "abc" typeof list should also be true
        val correct =
            (arg0 is PrologList && arg1.name == "list")
                ||
                (arg0 is PrologString && arg1.name == "string")
                ||
                (arg0.prologTypeName == arg1.name)

        if (correct) yield(Unification.TRUE)
    }
}
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
private fun typeCheckBuiltin(name: String, test: (Term) -> Boolean): Clause {
    return nativeRule(name, 1, getInvocationStackFrame()) { args, _ ->
        if (test(args[0])) yield(Unification.TRUE)
    }
}