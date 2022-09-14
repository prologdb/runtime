package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.MutableUnification
import com.github.prologdb.runtime.unification.Unification

@PrologTypeName("variable")
open class Variable(val name: String) : Term {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification {
        if (rhs is Variable && rhs == this) return Unification.TRUE

        val vars = MutableUnification.createTrue()
        vars.instantiate(this, rhs)
        return vars
    }

    override val variables: Set<Variable>
        get() = setOf(this)

    override val isGround = false

    override fun substituteVariables(mapper: (Variable) -> Term): Term {
        return mapper(this)
    }

    override fun toString(): String {
        return name
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Variable) return false

        if (name != other.name) return false

        return true
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    override fun compareTo(other: Term): Int {
        return when(other) {
            // standard prolog sorts variables by address
            // there is no such thing in kotlin; the closest
            // alternative on the JVM is System.identityHashCode.
            // however, on the JS platform, we're entirely lost.
            // therefore we just sort lexicographically by name
            is Variable -> name.compareTo(other.name)

            // Variables are always first
            else -> -1
        }
    }

    override var sourceInformation: PrologSourceInformation = NullSourceInformation

    companion object {
        @JvmStatic
        val ANONYMOUS: Variable = AnonymousVariable()
    }
}
