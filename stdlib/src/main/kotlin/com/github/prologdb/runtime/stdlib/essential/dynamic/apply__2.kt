package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinApply2 = nativeRule("apply", 2) { args, ctxt ->
    val arguments = args.getTyped<PrologList>(1)
    if (arguments.tail != null) {
        throw ArgumentError(1, "must not have a tail")
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
        else -> throw ArgumentTypeError(0, args[0], CompoundTerm::class.java, Atom::class.java)
    }
}
