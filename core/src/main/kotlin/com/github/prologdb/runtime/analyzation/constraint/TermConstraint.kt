package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

sealed class TermConstraint {
    abstract fun check(term: Term): Boolean

    abstract infix fun and(other: TermConstraint): TermConstraint

    abstract fun toString(forSubject: Variable): String
}

/**
 * All terms pass this constraint.
 */
object NoopConstraint : TermConstraint() {
    override fun check(term: Term) = true

    override fun and(other: TermConstraint) = other

    override fun toString(forSubject: Variable): String = "$forSubject = _"
}

/**
 * No term can pass this constraint.
 */
object ImpossibleConstraint : TermConstraint() {
    override fun check(term: Term) = false

    override fun and(other: TermConstraint) = this

    override fun toString(forSubject: Variable): String = "$forSubject cannot be satisfied"
}

class TypeTermConstraint(val type: Class<out Term>) : TermConstraint() {
    override fun check(term: Term) = type.isInstance(term)

    override fun and(other: TermConstraint): TermConstraint {
        return when(other) {
            is NoopConstraint -> this
            is ImpossibleConstraint -> other
            is TypeTermConstraint -> {
                when {
                    this.type.isAssignableFrom(other.type) -> other
                    other.type.isAssignableFrom(this.type) -> this
                    else -> ImpossibleConstraint
                }
            }
            is IdentityTermConstraint -> {
                if (check(other.literal)) other else ImpossibleConstraint
            }
        }
    }

    override fun toString(forSubject: Variable): String {
        return when {
            Atom::class.java.isAssignableFrom(type) -> "atom($forSubject)"
            PrologInteger::class.java.isAssignableFrom(type) -> "integer($forSubject)"
            PrologDecimal::class.java.isAssignableFrom(type) -> "decimal($forSubject)"
            PrologNumber::class.java.isAssignableFrom(type) -> "number($forSubject)"
            PrologString::class.java.isAssignableFrom(type) -> "string($forSubject)"
            PrologList::class.java.isAssignableFrom(type) -> "is_list($forSubject)"
            PrologDictionary::class.java.isAssignableFrom(type) -> "is_dict($forSubject)"
            CompoundTerm::class.java.isAssignableFrom(type) -> "typeof($forSubject, `compound term`)"
            else -> "$forSubject is instance of ${type.simpleName}"
        }
    }

    companion object {
        inline operator fun <reified T : Term> invoke() = TypeTermConstraint(T::class.java)
    }
}

/**
 * The term under inspection must exactly match another term
 */
class IdentityTermConstraint(val literal: Term) : TermConstraint() {
    override fun check(term: Term): Boolean = literal == term

    override fun and(other: TermConstraint): TermConstraint {
        return when (other) {
            is NoopConstraint -> this
            is ImpossibleConstraint -> other
            is TypeTermConstraint -> {
                if (other.type.isInstance(literal)) this else ImpossibleConstraint
            }
            is IdentityTermConstraint -> {
                if (literal == other.literal) this else ImpossibleConstraint
            }
        }
    }

    override fun toString(forSubject: Variable): String = "$forSubject == $literal"
}