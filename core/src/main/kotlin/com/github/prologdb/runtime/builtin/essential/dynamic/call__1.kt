package com.github.prologdb.runtime.builtin.essential.dynamic

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.proofsearch.PrologCallableFulfill
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.VariableBucket

internal val BuiltinCall1 = object : Rule(CompoundTerm("call", arrayOf(Variable("_Arg1"))), PredicateInvocationQuery(CompoundTerm("__nativeCode", emptyArray()))) {
    override val fulfill: PrologCallableFulfill = { args, ctxt ->
        val goalInput = args[0]

        val goalInputAsLambda = goalInput.tryCastToLambda()
        if (goalInputAsLambda != null) {
            goalInputAsLambda.fulfill(this, emptyArray(), ctxt)
        } else {
            val goal = when (goalInput) {
                is CompoundTerm -> goalInput
                is Atom -> CompoundTerm(goalInput.name, emptyArray())
                else -> throw PrologRuntimeException("Argument 1 given to call/1 must be an atom or a compound term, got ${goalInput.prologTypeName}")
            }

            ctxt.fulfillAttach(
                this,
                PredicateInvocationQuery(goal, goalInput.sourceInformation),
                VariableBucket()
            )
        }
    }
}
