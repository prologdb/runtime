package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.VariableBucket

internal val BuiltinCall1 = nativeRule("call", 1) { args, ctxt ->
    val goalInput = args[0]

    val goal = when (goalInput) {
        is CompoundTerm -> goalInput
        is Atom -> CompoundTerm(goalInput.name, emptyArray())
        else -> throw ArgumentTypeError(0, goalInput, Atom::class.java, CompoundTerm::class.java)
    }

    ctxt.fulfillAttach(
        this,
        PredicateInvocationQuery(goal, goalInput.sourceInformation),
        VariableBucket()
    )
}
