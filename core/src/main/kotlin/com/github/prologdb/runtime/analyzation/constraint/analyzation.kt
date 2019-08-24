package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.term.CompoundTerm
import mapToArray

class InvocationConstraint(val termConstraints: Array<out TermConstraint>) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is InvocationConstraint) return false

        if (!termConstraints.contentEquals(other.termConstraints)) return false

        return true
    }

    override fun hashCode(): Int {
        return termConstraints.contentHashCode()
    }
}

val CompoundTerm.unificationConditions: InvocationConstraint
    get() = InvocationConstraint(arguments.mapToArray(TermConstraint.Companion::unifiesWith))