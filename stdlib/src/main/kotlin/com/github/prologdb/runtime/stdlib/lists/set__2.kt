package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.ArgumentError
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
val BuiltinSet2 = nativeRule("set", 2) { args, _ ->
    if (args[0] is Variable) {
        val arg0 = args[0] as Variable
        val arg1 = args.getTyped<PrologList>(1)
        if (arg1.tail != null) {
            throw ArgumentError(1, "must not have a tail")
        }

        val isSet = arg1.elements.size == arg1.elements.toSet().size
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
            throw throw ArgumentError(0, "must not have a tail")
        }
        val arg1 = args.getTyped<Variable>(1)

        val arg0asSet = arg0.elements.toSet()
        val result = VariableBucket()
        result.instantiate(arg1, PrologList(arg0asSet.toList()))
        return@nativeRule Unification(result)
    }
}
