package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.*
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
 * To check for equality, the predicate with the name given in the third argument
 * is consulted; e.g. `set([1, 1, 2], X, ==)` would try to proof `=(1, 1)` to check
 * whether the two `1`s are equal.
 */
internal val Set3Builtin = nativeRule("set", 3, { args, context ->
    val arg0 = args[0]
    val arg1 = args[1]
    val comparatorName = args[2] as? Atom ?: throw PrologRuntimeException("Type error: argument 3 to set/3 must be an atom")

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

    if (arg0 !is Variable)
    {
        if (arg0 !is PrologList) throw PrologRuntimeException("Type error: argument 1 to set/1 must be of type list")
        if (arg0.tail is Variable) throw PrologRuntimeException("Type error: tail of argument 1 to set/1 not sufficiently instantiated")
        if (arg1 !is Variable) throw PrologRuntimeException("Type error: if argument 1 to set/1 is instantiated, argument 2 must be unbound")

        val arg0asSet = arg0.elements.toSetUsingComparator(comparatorName)
        val result = VariableBucket()
        result.instantiate(arg1, PrologList(arg0asSet.toList()))
        yield(Unification(result))
    }
    else
    {
        if (arg1 !is PrologList) throw PrologRuntimeException("Type error: if argument 1 to set/2 is unbound, argument 2 must be of type list")
        if (arg1.tail is Variable) throw PrologRuntimeException("Type error: tail of argument 2 to set/1 not sufficiently instantiated")

        val isSet = arg1.elements.size == arg1.elements.toSetUsingComparator(comparatorName).size
        if (isSet) {
            val result = VariableBucket()
            result.instantiate(arg0, arg1)
            yield(Unification(result))
        }
    }
})