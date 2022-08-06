package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinLimit2 = nativeRule("limit", 2) { args, ctxt ->
    val limit = args.getTyped<PrologInteger>(0)
    val goal = args.getQuery(1)

    val solutions = buildLazySequence(ctxt.principal) {
        ctxt.fulfillAttach(this, goal, VariableBucket())
    }

    yieldAllFinal(solutions.limitRemaining(limit.value))
}