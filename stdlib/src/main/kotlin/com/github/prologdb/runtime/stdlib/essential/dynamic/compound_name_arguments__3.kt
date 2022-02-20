package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.unify

val BuiltinCompoundNameArguments3 = nativeRule("compound_name_arguments", 3) { args, ctxt ->
    val compoundInput = args[0]

    if (compoundInput is CompoundTerm) {
        return@nativeRule arrayOf(args[1], args[2]).unify(arrayOf(
            Atom(compoundInput.functor),
            PrologList(compoundInput.arguments.asList())
        ), ctxt.randomVariableScope)
    }

    if (compoundInput !is Variable) {
        throw ArgumentTypeError(0, compoundInput, Variable::class.java, CompoundTerm::class.java)
    }

    val desiredFunctor = when (val desiredFunctorInput = args[1]) {
        is Atom -> desiredFunctorInput.name
        is PrologString -> desiredFunctorInput.toKotlinString()
        else -> throw ArgumentTypeError(1, desiredFunctorInput, Atom::class.java, PrologString::class.java)
    }

    val desiredArgumentsInput = args.getTyped<PrologList>(2)

    if (desiredArgumentsInput.tail != null) {
        throw PrologInvocationContractViolationException("If argument 1 is unbound, argument 3 must not have a tail")
    }

    return@nativeRule compoundInput.unify(CompoundTerm(desiredFunctor, desiredArgumentsInput.elements.toTypedArray()), ctxt.randomVariableScope)
}
