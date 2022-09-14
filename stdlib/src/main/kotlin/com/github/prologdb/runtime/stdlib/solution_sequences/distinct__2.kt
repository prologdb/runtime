package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.numberVariables
import com.github.prologdb.runtime.unification.Unification

val BuiltinDistinct2 = nativeRule("distinct", 2) { args, ctxt ->
    val goal = args.getQuery(1)
    val witness = args.get(0)

    yieldAllFinal(
        buildLazySequence(ctxt.principal) {
            ctxt.fulfillAttach(this, goal, Unification())
        }
            .distinctBy { unification ->
                val witnessInstantiated = witness
                    .substituteVariables(unification.variableValues.asSubstitutionMapper())

                witnessInstantiated.numberVariables()
            }
    )
}