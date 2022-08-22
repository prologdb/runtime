package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinDistinct2 = nativeRule("distinct", 2) { args, ctxt ->
    val goal = args.getQuery(1)
    val witness = args.get(0)

    yieldAllFinal(
        buildLazySequence(ctxt.principal) {
            ctxt.fulfillAttach(this, goal, VariableBucket())
        }
            .distinctBy { unification ->
                val witnessInstantiated = witness
                    .substituteVariables(unification.variableValues.asSubstitutionMapper())

                witnessInstantiated.numberVariables()
            }
    )
}

private fun Term.numberVariables(): Term {
    var variableCounter = 0L
    return substituteVariables { CompoundTerm("\$VAR", arrayOf(PrologInteger(variableCounter))) }
}