package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.VariableDiscrepancyException

val BuiltinCompoundNameArguments3 = nativeRule("compound_name_arguments", 3) { args, ctxt ->
    val compoundInput = args[0]

    if (compoundInput is CompoundTerm) {
        val functorResult = args[1].unify(Atom(compoundInput.functor), ctxt.randomVariableScope)
        if (functorResult != null) {
            val argumentsResult = args[2].unify(PrologList(compoundInput.arguments.asList()), ctxt.randomVariableScope)
            if (argumentsResult != null) {
                try {
                    functorResult.variableValues.incorporate(argumentsResult.variableValues)
                    yield(functorResult)
                } catch (ex: VariableDiscrepancyException) {
                    /* does not unify, no yield */
                }
            }
        }
    } else if (compoundInput !is Variable) {
        throw PrologRuntimeException("Argument 1 to compound_name_arguments/3 must be unbound or a compound term, got ${compoundInput.prologTypeName}")
    }

    val desiredFunctorInput = args[1]
    val desiredArgumentsInput = args[2]

    val desiredFunctor = when (desiredFunctorInput) {
        is Atom -> desiredFunctorInput.name
        is PrologString -> desiredFunctorInput.toKotlinString()
        else -> throw PrologRuntimeException("Argument 2 to compound_name_arguments/3 must be unbound, an atom or a string, got ${desiredFunctorInput.prologTypeName}")
    }

    if (desiredArgumentsInput !is PrologList) {
        throw PrologRuntimeException("Argument 3 to compound_name_arguments/3 must be unbound or a list, got ${desiredArgumentsInput.prologTypeName}")
    }

    if (desiredArgumentsInput.tail != null) {
        throw PrologRuntimeException("If argument 1 to compound_name_arguments/3 is unbound, argument 3 must not have a tail")
    }

    return@nativeRule compoundInput.unify(CompoundTerm(desiredFunctor, desiredArgumentsInput.elements.toTypedArray()), ctxt.randomVariableScope)
}
