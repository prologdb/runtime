package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope

@PrologTypeName("atom")
class Atom(val name: String) : Term {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        when (rhs) {
            this        -> return Unification.TRUE
            is Variable -> return rhs.unify(this, randomVarsScope)
            else        -> return Unification.FALSE
        }
    }

    override val variables: Set<Variable> = emptySet()
    override val isGround = true
    override fun substituteVariables(mapper: (Variable) -> Term) = this

    override fun compareTo(other: Term): Int {
        return when (other) {
            // variables, numbers and strings are, by category, lesser than atoms
            is Variable, is PrologNumber, is PrologString -> 1

            // lexicographical order
            is Atom -> this.name.compareTo(other.name)

            // everything else is, by category, greater than atoms
            else -> return -1
        }
    }

    override fun toString(): String {
        val firstChar = name[0]
        return if (firstChar !in '0' .. '9' && (firstChar.uppercaseChar() == firstChar || name.contains(Regex("\\s")))) {
            "'$name'"
        } else {
            name
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Atom) return false

        if (name != other.name) return false

        return true
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    override var sourceInformation: PrologSourceInformation = NullSourceInformation

    /** Whether this atom appeared in quotes. Important for the parser mostly. */
    var quoted: Boolean = false
}
