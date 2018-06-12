package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.Number
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

/**
 * Implements the is/2 builtin that evaluates mathematical expressions
 */
internal val BuiltinIs = prologBuiltin("is", 2) { args, _, _ ->
    val inputForA = args[0]
    val inputForB = args[1]

    if (inputForA is Variable) {
        return@prologBuiltin LazySequence.of(inputForA.unify(inputForB.asNumber))
    }

    if (inputForB is Variable) {
        return@prologBuiltin LazySequence.of(inputForB.unify(inputForA.asNumber))
    }

    if (inputForA is Number) {
        return@prologBuiltin LazySequence.ofNullable(
            Unification.whether(
                inputForB.asNumber == inputForA
            )
        )
    }

    return@prologBuiltin Unification.NONE
}