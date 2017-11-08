package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.RandomVariableScope

open class Atom(val name: String) : Term {

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs == this) {
            return Unification.TRUE
        }
        else if (rhs is Variable) {
            return rhs.unify(this)
        }
        else
        {
            return Unification.FALSE
        }
    }

    override val variables: Set<Variable> = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as Atom

        if (name != other.name) return false

        return true
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    override fun toString(): String {
        val firstChar = name[0]
        if (firstChar !in '0' .. '9' && (firstChar.toUpperCase() == firstChar || name.contains(Regex("\\s")))) {
            return "'$name'"
        }
        else {
            return name
        }
    }
}