package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.VariableDiscrepancyException
import kotlin.math.max

val BuiltinReverse2 = nativeRule("reverse", 2) { args, ctxt ->
    val arg1 = args.getTypedOrUnbound<PrologList>(0)
    val arg2 = args.getTypedOrUnbound<PrologList>(1)

    val listToReverse: PrologList
    val unifyWithReversed: Term

    if (arg1 is PrologList && arg2 is PrologList) {
        if (arg1.tail == null && arg2.tail != null) {
            return@nativeRule reverseWithoutTailAndUnify(arg1, arg2, ctxt.randomVariableScope)
        } else if (arg1.tail != null && arg2.tail == null) {
            return@nativeRule reverseWithoutTailAndUnify(arg2, arg1, ctxt.randomVariableScope)
        } else {
            listToReverse = arg1
            unifyWithReversed = arg2
        }
    } else if (arg1 is Variable) {
        if (arg2 is Variable) {
            throw ArgumentTypeError(1, arg2, PrologList::class.java)
        }

        listToReverse = arg2 as PrologList
        unifyWithReversed = arg1
    } else {
        listToReverse= arg1 as PrologList
        unifyWithReversed = arg2
    }

    if (listToReverse.tail == null) {
        return@nativeRule reverseWithoutTailAndUnify(listToReverse, unifyWithReversed, ctxt.randomVariableScope)
    }

    // there are infinite solutions because both the input and the output are unbound/tailed

    val reversedList = PrologList(listToReverse.elements.asReversed())
    var prefixLength = if (unifyWithReversed is PrologList) max(reversedList.elements.size, unifyWithReversed.elements.size) - 1 else 0
    prefixLength -= 1 // so that we can do continue in the loop
    while (true) {
        prefixLength++

        val elements = ArrayList<Term>(prefixLength + reversedList.elements.size)
        repeat(prefixLength) {
            elements.add(ctxt.randomVariableScope.createNewRandomVariable())
        }
        elements.addAll(reversedList.elements)

        val stepResult = PrologList(elements, null).unify(unifyWithReversed, ctxt.randomVariableScope)
            ?: continue

        if (unifyWithReversed !is PrologList || unifyWithReversed.tail == null) {
            yield(stepResult)
        } else {
            val fullList = unifyWithReversed.substituteVariables(stepResult.variableValues.asSubstitutionMapper())
            check(fullList.tail == null)

            val partialResult1 = unifyWithReversed.unify(fullList, ctxt.randomVariableScope)
                ?: continue
            val partialResult2 = PrologList(fullList.elements.asReversed()).unify(listToReverse, ctxt.randomVariableScope)
                ?: continue
            try {
                yield(partialResult1.combinedWith(partialResult2, ctxt.randomVariableScope))
            }
            catch (ex: VariableDiscrepancyException) { }
        }
    }

    @Suppress("UNREACHABLE_CODE")
    // this return is necessary to make the Kotlin 1.7.10 compiler happy
    return@nativeRule Unification.FALSE
}

private fun reverseWithoutTailAndUnify(listToReverse: PrologList, unifyWith: Term, randomVariableScope: RandomVariableScope): Unification? {
    check(listToReverse.tail == null)
    val reversedList = PrologList(listToReverse.elements.asReversed())
    return reversedList.unify(unifyWith, randomVariableScope)
}