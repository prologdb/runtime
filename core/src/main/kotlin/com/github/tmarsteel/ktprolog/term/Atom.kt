package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.knowledge.RandomVariableScope

class Atom(val name: String) : Term {

    override fun unify(rhs: Term, randomVariableScope: RandomVariableScope): Unification? {
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

    override fun toString() = if (name[0].isLowerCase()) name else "'$name'"
}