package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

private const val IOTA_BATCHSIZE = 100

/**
 * iota(:Target, ++StartInclusive, ++EndExclusive)
 *
 * Steps from StartInclusive to EndExclusive in steps of one. Et every steps, yields a solution that
 * instantiates Target to the current value. For example:
 *
 *     ?- iota(X, 0, 3).
 *     X = 0 ;
 *     X = 1 ;
 *     X = 2 ;
 *     false .
 *
 *     ?- findall(X, iota(X, 8, 2), R).
 *     R = [8, 7, 6, 5, 4, 3] .
 */
val BuiltinIota3 = nativeRule("iota", 3) { args, ctxt ->
    val target  = args[0]
    val start   = args[1] as? PrologInteger ?: throw PrologRuntimeException("Argument 1 to iota/2 must be an integer, got ${args[1].prologTypeName}")
    val endExcl = args[2] as? PrologInteger ?: throw PrologRuntimeException("Argument 2 to iota/2 must be an integer, got ${args[2].prologTypeName}")

    if (start == endExcl) {
        // empty range
        return@nativeRule
    }

    if (target !is Variable) {
        // target is bound, this really is a range check
        if (target is PrologNumber) {
            if (target >= start && target < endExcl) {
                yield(Unification.TRUE)
            }
        }
        // else: will not unify, ever
        return@nativeRule
    }

    // implicit: target as Variable

    val progression = if (start > endExcl) {
        LongProgression.fromClosedRange(start.toInteger(), endExcl.toInteger() + 1, -1)
    } else {
        LongProgression.fromClosedRange(start.toInteger(), endExcl.toInteger() - 1, 1)
    }

    // yield in batches (as a trade-off in performance vs memory)
    val batchStorage = ArrayList<Unification>(IOTA_BATCHSIZE)
    val source = progression.iterator()
    do {
        batchStorage.clear()
        while (source.hasNext() && batchStorage.size < IOTA_BATCHSIZE) {
            val nextN = source.next()
            batchStorage.add(target.unify(PrologInteger.createUsingStringOptimizerCache(nextN), ctxt.randomVariableScope))
        }
        yieldAll(batchStorage)
    }
    while(source.hasNext())
}
