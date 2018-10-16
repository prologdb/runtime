package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

/**
 * Implements the is/2 builtin that evaluates mathematical expressions
 */
internal val BuiltinIs = prologBuiltin("is", 2) { args, _, _ ->
    val inputForA = args[0]
    val inputForB = args[1]

    if (inputForA is Variable) {
        return@prologBuiltin LazySequence.of(inputForA.unify(inputForB.asPrologNumber))
    }

    if (inputForB is Variable) {
        return@prologBuiltin LazySequence.of(inputForB.unify(inputForA.asPrologNumber))
    }

    if (inputForA is PrologNumber) {
        return@prologBuiltin LazySequence.ofNullable(
            Unification.whether(
                inputForB.asPrologNumber == inputForA
            )
        )
    }

    return@prologBuiltin Unification.NONE
}