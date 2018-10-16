package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification

internal val BuiltinAtomString = prologBuiltin("atom_string", 2) { args, _, randomVarsScope ->
    val inputForA = args[0]
    val inputForB = args[1]

    if (inputForA is Variable && inputForB is Variable) {
        throw PrologRuntimeException("Type Error: arguments not sufficiently instantiated")
    }

    fun convertInputForAToKotlinString(): String {
        return when (inputForA) {
            is Atom -> inputForA.name
            is PrologInteger -> inputForA.value.toString()
            is PrologDecimal -> inputForA.value.toString()
            else -> throw PrologRuntimeException("Type Error: first argument to atom_string/2 must be an atom or a number, ${inputForA.prologTypeName} given")
        }
    }

    if (inputForB !is Variable) {
        if (inputForB !is PrologString) {
            throw PrologRuntimeException("Type Error: second argument to atom_string/2 must be a string, ${inputForB.prologTypeName} given")
        }

        if (inputForA is Variable) {
            return@prologBuiltin LazySequence.ofNullable(inputForA.unify(Atom(inputForB.toKotlinString()), randomVarsScope))
        } else {
            return@prologBuiltin LazySequence.ofNullable(Unification.whether(convertInputForAToKotlinString() == inputForB.toKotlinString()))
        }
    }

    // implicit: b is not instantiated
    return@prologBuiltin LazySequence.ofNullable(inputForB.unify(PrologString(convertInputForAToKotlinString())))
}