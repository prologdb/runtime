package com.github.prologdb.runtime.analyzation.constraint

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