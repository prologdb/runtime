package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

val TypeofBuiltin = nativeRule("typeof", 2) { args, ctxt ->
    val arg0 = args[0]
    val arg1 = args[1]

    if (arg1 is Variable) {
        val actualValueArg1 = Atom(arg0.prologTypeName)
        return@nativeRule arg1.unify(actualValueArg1, ctxt.randomVariableScope)
    } else {
        if (arg1 !is Atom) throw PrologRuntimeException("Type error: argument 2 to typeof/2 must be an atom or unbound")

        // typecheck; "abc" typeof list should also be true
        val correct =
            (arg0 is PrologList && arg1.name == "list")
                ||
                (arg0 is PrologString && arg1.name == "string")
                ||
                (arg0.prologTypeName == arg1.name)

        return@nativeRule Unification.whether(correct)
    }
}
val TypeSafetyModule = nativeModule("typesafety") {
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
    add(typeCheckBuiltin("nonground") { it.variables.isNotEmpty() })

    add(TypeofBuiltin)
}

/**
 * @return a clause with the given functor and arity 1 that succeeds if the first argument passes
 * the given predicate.
 */
private fun typeCheckBuiltin(name: String, test: (Term) -> Boolean): Rule {
    return nativeRule(name, 1, getInvocationStackFrame()) { args, _ -> Unification.whether(test(args[0])) }
}
