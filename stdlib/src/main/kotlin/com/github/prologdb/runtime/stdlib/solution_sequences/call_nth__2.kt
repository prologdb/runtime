package com.github.prologdb.runtime.stdlib.solution_sequences

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemainingIndexedNotNull
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.VariableBucket
import com.github.prologdb.runtime.unification.VariableDiscrepancyException

val BuiltinCallNth2 = nativeRule("call_nth", 2) { args, ctxt ->
    val goal = args.getQuery(0)
    val nInput = args.get(1)

    val solutions = buildLazySequence(ctxt.principal) {
        ctxt.fulfillAttach(this, goal, VariableBucket())
    }

    if (nInput is PrologInteger) {
        return@nativeRule yieldAllFinal(solutions.skipRemaining(nInput.value - 1).limitRemaining(1))
    }

    if (nInput !is Variable) {
        throw ArgumentTypeError(1, nInput, PrologInteger::class.java, Variable::class.java)
    }

    yieldAllFinal(
        solutions.mapRemainingIndexedNotNull { index, unification ->
            val indexUnification = nInput.unify(PrologInteger(index), ctxt.randomVariableScope)
            return@mapRemainingIndexedNotNull try {
                indexUnification.combinedWith(unification, ctxt.randomVariableScope)
            }
            catch (ex: VariableDiscrepancyException) {
                null
            }
        }
    )
}