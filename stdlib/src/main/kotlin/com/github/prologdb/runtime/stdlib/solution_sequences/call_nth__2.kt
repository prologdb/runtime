package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemainingIndexedNotNull
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.asIntegerInRange
import com.github.prologdb.runtime.unification.Unification

val BuiltinCallNth2 = nativeRule("call_nth", 2) { args, ctxt ->
    val goal = args.getQuery(0)
    val nInput = args[1]

    val solutions = buildLazySequence(ctxt.principal) {
        ctxt.fulfillAttach(this, goal, Unification.TRUE)
    }

    if (nInput is PrologNumber) {
        val nTh = nInput.asIntegerInRange(0..Long.MAX_VALUE)
            ?: throw ArgumentError(1, "Must be an integer in range [0; ${Long.MAX_VALUE}], got $nInput")
        return@nativeRule yieldAllFinal(solutions.skipRemaining(nTh - 1).limitRemaining(1))
    }

    if (nInput !is Variable) {
        throw ArgumentTypeError(1, nInput, PrologNumber::class.java, Variable::class.java)
    }

    yieldAllFinal(
        solutions.mapRemainingIndexedNotNull { index, unification ->
            val indexUnification = nInput.unify(PrologNumber(index), ctxt.randomVariableScope)
            return@mapRemainingIndexedNotNull indexUnification.combinedWith(unification, ctxt.randomVariableScope)
        }
    )
}