package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

/**
 * Implements the is/2 builtin that evaluates mathematical expressions
 */
internal val BuiltinIs = prologBuiltin("is", 2) { args, _ ->
    val inputForA = args[0]
    val inputForB = args[1]

    if (inputForA is Variable) {
        yield(inputForA.unify(inputForB.asPrologNumber))
    }

    if (inputForB is Variable) {
        yield(inputForB.unify(inputForA.asPrologNumber))
    }

    if (inputForA is PrologNumber) {
        if (inputForB.asPrologNumber == inputForA) yield(Unification.TRUE)
    }
}