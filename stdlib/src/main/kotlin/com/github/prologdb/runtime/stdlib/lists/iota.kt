package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Variable

private const val IOTA_BATCHSIZE = 100

/**
 * iota(:Target, ++StartInclusive, ++EndExclusive)
 *
 * Steps from StartInclusive to EndExclusive in steps of one. At every step, yields a solution that
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
    val target = args[0]
    val start = args.getInteger(1)
    val endExcl = args.getInteger(2)

    if (start == endExcl) {
        // empty range
        return@nativeRule null
    }

    if (target !is Variable) {
        // target is bound, this really is a range check
        if (target is PrologNumber) {
            if (target >= start && target < endExcl) {
                yield(Unification.TRUE)
            }
        }
        // else: will not unify, ever
        return@nativeRule null
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
            batchStorage.add(target.unify(PrologNumber(nextN), ctxt.randomVariableScope))
        }
        if (source.hasNext()) {
            yieldAll(batchStorage)
        } else {
            return@nativeRule yieldAllFinal(batchStorage.asSequence())
        }
    }
    while(source.hasNext())

    return@nativeRule null
}

/**
 * iota(:Target, ++StartInclusive, ++EndExclusive, ++Step)
 *
 * Steps from StartInclusive to EndExclusive in steps of Step. At every step, yields a solution that
 * instantiates Target to the current value. For example:
 *
 *     ?- iota(X, 0, 3, 0.5).
 *     X = 0 ;
 *     X = 0.5 ;
 *     X = 1 ;
 *     X = 1.5 ;
 *     X = 2 ;
 *     X = 2.5 ;
 *     false .
 *
 *     ?- iota(X, 0, 2, 0.7).
 *     X = 0 ;
 *     X = 0.7 ;
 *     X = 1.4 ;
 *     false .
 */
val BuiltinIota4 = nativeRule("iota", 4) { args, ctxt ->
    val target = args[0]
    val start = args.getTyped<PrologNumber>(1)
    val endExcl = args.getTyped<PrologNumber>(2)
    val step = args.getTyped<PrologNumber>(3)

    if ((step.isInteger && step.toInteger() == 0L) || step.toDecimal() == 0.0) {
        throw ArgumentError(3, "must not be 0.")
    }

    if (start == endExcl) {
        // empty range
        return@nativeRule null
    }

    if (target !is Variable) {
        // target is bound, this really is a range check
        return@nativeRule if (target is PrologNumber && target >= start && target < endExcl) {
            Unification.TRUE
        } else {
            Unification.FALSE
        }
    }

    // implicit: target as Variable

    val isForward = start < endExcl

    val progression = generateSequence(start) { carry ->
        val next = carry.plus(step, ctxt.mathContext)
        if (next == carry) {
            // step is too small in comparison to range
            throw ArgumentError(3, "step is too small for the range (IEEE 754 inprecision)")
        }
        // break condition
        if (isForward && next >= endExcl) return@generateSequence null
        if (!isForward && next <= endExcl) return@generateSequence null

        next
    }

    // yield in batches (as a trade-off in performance vs memory)
    val batchStorage = ArrayList<Unification>(IOTA_BATCHSIZE)
    val source = progression.iterator()
    do {
        batchStorage.clear()
        while (source.hasNext() && batchStorage.size < IOTA_BATCHSIZE) {
            val nextN = source.next()
            batchStorage.add(target.unify(nextN, ctxt.randomVariableScope))
        }
        if (source.hasNext()) {
            yieldAll(batchStorage)
        } else {
            return@nativeRule yieldAllFinal(batchStorage.asSequence())
        }
    }
    while(source.hasNext())

    null
}
