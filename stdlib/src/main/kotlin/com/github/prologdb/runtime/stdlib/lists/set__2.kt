package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

/**
 * set(-List, -List)
 *
 * Fulfills if the second argument does not have duplicates and intersection between
 * the arguments is equal to the second argument, ignoring order.
 *
 * Exactly one of the arguments must be instantiated; the other one must be unbound.
 *
 * This acts as if defined as `set(L, S) :- set(L, S, ==).`
 */
internal val BuiltinSet2 = nativeRule("set", 2) { args, _ ->
    val arg0 = args[0]
    val arg1 = args[1]

    if (arg0 !is Variable) {
        if (arg0 !is PrologList) throw PrologRuntimeException("Type error: argument 1 to set/1 must be of type list")
        if (arg0.tail is Variable) throw PrologRuntimeException("Type error: tail of argument 1 to set/1 not sufficiently instantiated")
        if (arg1 !is Variable) throw PrologRuntimeException("Type error: if argument 1 to set/1 is instantiated, argument 2 must be unbound")

        val arg0asSet = arg0.elements.toSet()
        val result = VariableBucket()
        result.instantiate(arg1, PrologList(arg0asSet.toList()))
        return@nativeRule Unification(result)
    }
    else
    {
        if (arg1 !is PrologList) throw PrologRuntimeException("Type error: if argument 1 to set/2 is unbound, argument 2 must be of type list")
        if (arg1.tail is Variable) throw PrologRuntimeException("Type error: tail of argument 2 to set/1 not sufficiently instantiated")

        val isSet = arg1.elements.size == arg1.elements.toSet().size
        return@nativeRule if (isSet) {
            val result = VariableBucket()
            result.instantiate(arg0, arg1)
            Unification(result)
        } else {
            null
        }
    }
}
