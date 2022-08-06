package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinOffset2 = nativeRule("offset", 2) { args, ctxt ->
    val offset = args.getTyped<PrologInteger>(0)
    val goal = args.getQuery(1)

    val solutions = buildLazySequence(ctxt.principal) {
        ctxt.fulfillAttach(this, goal, VariableBucket())
    }

    solutions.skip(offset.value)
    yieldAllFinal(solutions)
}