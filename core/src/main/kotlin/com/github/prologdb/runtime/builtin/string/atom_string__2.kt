package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.builtin.A
import com.github.prologdb.runtime.builtin.B
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.term.Decimal as PrologDecimal
import com.github.prologdb.runtime.term.Integer as PrologInteger

object AtomStringPredicate : Predicate("atom_string", arrayOf(A, B)) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs !is Predicate) return Unification.FALSE
        if (rhs.arity != this.arity || rhs.name != this.name) return Unification.FALSE

        val inputForA = rhs.arguments[0]
        val inputForB = rhs.arguments[1]

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
                return inputForA.unify(inputForB, randomVarsScope)
            } else {
                return Unification.whether(convertInputForAToKotlinString() == inputForB.toKotlinString())
            }
        }

        // implicit: b is not instantiated
        return inputForB.unify(PrologString(convertInputForAToKotlinString()))
    }
}
