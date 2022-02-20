package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

/**
 * set(-List, -List, +Atom)
 *
 * Fulfills if the second argument does not have duplicates and intersection between
 * the arguments is equal to the second argument, ignoring order.
 *
 * Of the first two arguments, exactly one must be instantiated; the other one must be unbound.
 *
 * To check for equality, the predicate with the functor given in the third argument
 * is consulted; e.g. `set([1, 1, 2], X, ==)` would try to proof `==(1, 1)` to check
 * whether the two `1`s are equal.
 */
val BuiltinSet3 = nativeRule("set", 3) { args, context ->
    val comparatorName = args.getTyped<Atom>(2)

    fun <T : Term> Collection<T>.toSetUsingComparator(comparatorName: Atom): List<T> {
        fun Term.isEqualToAccordingToComparator(rhs: Term): Boolean {
            val result = buildLazySequence<Unification>(principal) { context.fulfillAttach(this, PredicateInvocationQuery(CompoundTerm(comparatorName.name, arrayOf(this@isEqualToAccordingToComparator, rhs))), VariableBucket()) }
            val areEqual = result.tryAdvance() != null
            result.close()
            return areEqual
        }

        val setList = mutableListOf<T>()
        for (e in this) {
            if (setList.none { it.isEqualToAccordingToComparator(e) }) {
                setList.add(e)
            }
        }

        return setList
    }

    if (args[0] is Variable) {
        val arg0 = args[0] as Variable
        val arg1 = args.getTyped<PrologList>(1)
        if (arg1.tail != null) {
            throw ArgumentError(1, "must not have a tail")
        }

        val isSet = arg1.elements.size == arg1.elements.toSetUsingComparator(comparatorName).size
        return@nativeRule if (isSet) {
            val result = VariableBucket()
            result.instantiate(arg0, arg1)
            Unification(result)
        } else {
            null
        }
    } else {
        val arg0 = args.getTyped<PrologList>(0)
        if (arg0.tail != null) {
            throw ArgumentError(0,"must not have a tail")
        }
        val arg1 = args.getTyped<Variable>(1)

        val arg0asSet = arg0.elements.toSetUsingComparator(comparatorName)
        val result = VariableBucket()
        result.instantiate(arg1, PrologList(arg0asSet.toList()))
        return@nativeRule Unification(result)
    }
}
