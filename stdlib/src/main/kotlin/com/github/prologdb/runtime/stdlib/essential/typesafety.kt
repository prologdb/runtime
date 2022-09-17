package com.github.prologdb.runtime.stdlib.essential

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.launchWorkableFuture
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.stdlib.NativeCodeRule
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

val BuiltinTypeof2 = nativeRule("typeof", 2) { args, ctxt ->
    val arg0 = args[0]
    val arg1 = args[1]

    if (arg1 is Variable) {
        val actualValueArg1 = Atom(arg0.prologTypeName)
        return@nativeRule arg1.unify(actualValueArg1, ctxt.randomVariableScope)
    } else {
        if (arg1 !is Atom) throw ArgumentTypeError(1, arg1, Atom::class.java, Variable::class.java)

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

val BuiltinAtom1 = typeCheckBuiltin("atom") { it is Atom }

val BuiltinInteger1 = typeCheckBuiltin("integer") { it is PrologNumber && it.isInteger }
val BuiltinDecimal1 = typeCheckBuiltin("decimal") { it is PrologNumber && !it.isInteger }
val BuiltinNumber1 = typeCheckBuiltin("number") { it is PrologNumber }

val BuiltinString1 = typeCheckBuiltin("string") { it is PrologString }

val BuiltinIsList1 = typeCheckBuiltin("is_list") { it is PrologList }
val BuiltinIsDict1 = typeCheckBuiltin("is_dict") { it is PrologDictionary }

val BuiltinVar1 = typeCheckBuiltin("var") { it is Variable }
val BuiltinNonVar1 = typeCheckBuiltin("nonvar") { it !is Variable }

val BuiltinGround1 = typeCheckBuiltin("ground") { it.variables.isEmpty() }
val BuiltinNonGround1 = typeCheckBuiltin("nonground") { it.variables.isNotEmpty() }

/**
 * @return a clause with the given functor and arity 1 that succeeds if the first argument passes
 * the given predicate.
 */
private fun typeCheckBuiltin(name: String, test: (Term) -> Boolean): NativeCodeRule {
    return nativeRule(name, 1, getInvocationStackFrame()) { args, _ -> Unification.whether(test(args[0])) }
}

val BuiltinRequire2 = nativeRule("require", 2) { args, ctxt ->
    val goal = args.getQuery(0)
    val errorMessage = args.getTyped<PrologString>(1)

    val hadResults = await(launchWorkableFuture(ctxt.principal) {
        return@launchWorkableFuture foldRemaining(
            buildLazySequence(ctxt.principal) {
                ctxt.fulfillAttach(this, goal, Unification.TRUE)
            }
                .limitRemaining(1),
            false,
        ) { _, _ -> true }
    })

    if (!hadResults) {
        throw PrologInvocationContractViolationException(errorMessage.toKotlinString())
    }

    return@nativeRule Unification.TRUE
}