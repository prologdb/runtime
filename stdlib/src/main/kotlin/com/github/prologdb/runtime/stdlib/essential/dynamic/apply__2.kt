package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinApply2 = nativeRule("apply", 2) { args, ctxt ->
    val arguments = args[1] as? PrologList
        ?: throw PrologRuntimeException("Argument 2 to apply/2 must be a list, got ${args[1].prologTypeName}")
    if (arguments.tail != null) {
        throw PrologRuntimeException("Argument 2 to apply/2 must not have a tail.")
    }

    when (val targetInput = args[0]) {
        is CompoundTerm -> {
            val actualGoal = CompoundTerm(targetInput.functor, (targetInput.arguments.asList() + arguments.elements).toTypedArray())
            ctxt.fulfillAttach(this, PredicateInvocationQuery(actualGoal, targetInput.sourceInformation), VariableBucket())
        }
        is Atom -> {
            val actualGoal = CompoundTerm(targetInput.name, arguments.elements.toTypedArray())
            ctxt.fulfillAttach(this, PredicateInvocationQuery(actualGoal, targetInput.sourceInformation), VariableBucket())
        }
        else -> throw PrologRuntimeException("Argument 1 to apply/2 must be a compound term or an atom, got ${targetInput.prologTypeName}")
    }
}