package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologDictionary
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
        yield(arg1.unify(actualValueArg1, ctxt.randomVariableScope))
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
}.apply {
    behavesSemiDeterministic()
}

val CompoundBuiltin = nativeRule("compound", 1) { args, ctxt ->
    val arg0 = args[0]

    if (arg0 is CompoundTerm || arg0 is PrologList || arg0 is PrologDictionary) {
        yield(Unification.TRUE)
    }
}.apply {
    behavesSemiDeterministic()
}

val TypeSafetyModule = nativeModule("typesafety") {
    // all of these are /1 tests
    add(typeCheckBuiltin<Atom>("atom"))

    add(typeCheckBuiltin<PrologInteger>("integer"))
    add(typeCheckBuiltin<PrologDecimal>("decimal"))
    add(typeCheckBuiltin<PrologDecimal>("float"))
    add(typeCheckBuiltin<PrologNumber>("number"))

    add(typeCheckBuiltin<PrologString>("string"))

    add(typeCheckBuiltin<PrologList>("is_list"))

    add(typeCheckBuiltin<Variable>("var"))
    add(typeCheckBuiltin<Variable>("nonvar", negated = true))

    add(testingBuiltin("ground") { it.variables.isEmpty() })
    add(testingBuiltin("nonground") { it.variables.isNotEmpty() })

    add(TypeofBuiltin)
    add(CompoundBuiltin)
}

private inline fun <reified T : Term> typeCheckBuiltin(name: String, negated: Boolean = false)  = typeCheckBuiltin(name, T::class.java, negated)

/**
 * @return a clause with the given functor and arity 1 that succeeds if the first argument passes
 * the given predicate.
 */
private fun typeCheckBuiltin(name: String, type: Class<out Term>, negated: Boolean): Rule {
    return nativeRule(name, 1, getInvocationStackFrame(), if (negated)
        { args, _ -> if (!type.isInstance(args[0])) yield(Unification.TRUE) }
        else { args, _ -> if (type.isInstance(args[0])) yield(Unification.TRUE) }
    ).apply {
        behavesSemiDeterministic()
    }
}

private fun testingBuiltin(name: String, test: (Term) -> Boolean): Rule {
    return nativeRule(name, 1, getInvocationStackFrame()) { args, _ ->
        if (test(args[0])) yield(Unification.TRUE)
    }.apply {
        behavesSemiDeterministic()
    }
}
