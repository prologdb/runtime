package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.util.allIndexed
import com.github.prologdb.runtime.util.toRandomAccessList
import kotlin.math.max

sealed class TermConstraint {
    abstract fun check(term: Term): Boolean

    abstract infix fun and(other: TermConstraint): TermConstraint

    companion object {
        @JvmStatic
        fun unifiesWith(term: Term): TermConstraint = when(term) {
            is PrologList -> ListConstraint.unifiesWith(term)
            else -> TODO()
        }
    }
}

/**
 * All terms pass this constraint.
 */
object NoopConstraint : TermConstraint() {
    override fun check(term: Term) = true

    override fun and(other: TermConstraint) = other
}

/**
 * No term can pass this constraint.
 */
object ImpossibleConstraint : TermConstraint() {
    override fun check(term: Term) = false

    override fun and(other: TermConstraint) = this
}

class UnionTermConstraint private constructor(val constraints: List<TermConstraint>) : TermConstraint() {
    override fun check(term: Term): Boolean = constraints.any { it.check(term) }

    override fun and(other: TermConstraint): TermConstraint {
        val combined = constraints
            .asSequence()
            .map { it.and(other) }
            .filter { it is ImpossibleConstraint }
            .toList()

        if (combined.isEmpty()) return ImpossibleConstraint
        if (combined.any { it is NoopConstraint }) return NoopConstraint

        return UnionTermConstraint(combined)
    }

    companion object {
        @JvmStatic
        @JvmName("of")
        operator fun invoke(constraints: List<TermConstraint>): TermConstraint {
            return when {
                constraints.isEmpty() -> ImpossibleConstraint
                constraints.size == 1 -> constraints.first()
                constraints.any { it is NoopConstraint } -> NoopConstraint
                else -> (::UnionTermConstraint)(constraints)
            }
        }
    }
}

class TypeCheckConstraint(val type: Class<out Term>) : TermConstraint() {
    override fun check(term: Term) = type.isInstance(term)

    override fun and(other: TermConstraint): TermConstraint {
        return when(other) {
            is NoopConstraint -> this
            is ImpossibleConstraint -> other
            is TypeCheckConstraint -> {
                if (this.type.isAssignableFrom(other.type)) {
                    other
                } else {
                    this
                }
            }
            is ListConstraint -> {
                if (PrologList::class.java.isAssignableFrom(type)) {
                    other
                } else {
                    ImpossibleConstraint
                }
            }
            is IdentityTermConstraint -> {
                if (check(other.literal)) other else ImpossibleConstraint
            }
            is UnionTermConstraint -> other.and(this)
        }
    }

    companion object {
        inline operator fun <reified T : Term> invoke() = TypeCheckConstraint(T::class.java)
    }
}

class ListConstraint(
    elementConstraints: List<TermConstraint>,

    /**
     * Whether more entries than [nEntries] are allowed.
     */
    val moreAllowed: Boolean
) : TermConstraint() {

    val elementConstraints = elementConstraints.toRandomAccessList()

    override fun check(term: Term): Boolean {
        if (term !is PrologList) return false

        if (term.elements.size < elementConstraints.size) return false
        if (!moreAllowed && term.elements.size > elementConstraints.size) return false

        return term.elements.allIndexed { index, element -> elementConstraints[index].check(element) }
    }

    override fun and(other: TermConstraint): TermConstraint {
        when(other) {
            is NoopConstraint -> return this
            is ImpossibleConstraint -> return other
            is ListConstraint -> {
                if (other.elementConstraints.size > this.elementConstraints.size) {
                    return other.and(this)
                }

                // this' size is always equal to or greater than that of rhs

                if (this.elementConstraints.size > other.elementConstraints.size && !other.moreAllowed) {
                    return ImpossibleConstraint
                }

                val resultingElements = ArrayList<TermConstraint>(max(this.elementConstraints.size, other.elementConstraints.size))
                for (index in 0..this.elementConstraints.lastIndex) {
                    val otherConstraint = if (index > other.elementConstraints.lastIndex) NoopConstraint else other.elementConstraints[index]
                    resultingElements.add(otherConstraint.and(this.elementConstraints[index]))
                }

                return ListConstraint(resultingElements, this.moreAllowed && other.moreAllowed)
            }
            is IdentityTermConstraint -> {
                return if (other.literal is PrologList && check(other.literal)) other else ImpossibleConstraint
            }
            is TypeCheckConstraint,
            is UnionTermConstraint -> return other.and(this)
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ListConstraint) return false

        if (moreAllowed != other.moreAllowed) return false
        if (elementConstraints != other.elementConstraints) return false

        return true
    }

    override fun hashCode(): Int {
        var result = moreAllowed.hashCode()
        result = 31 * result + elementConstraints.hashCode()
        return result
    }

    companion object {
        @JvmStatic
        fun unifiesWith(term: PrologList) = ListConstraint(
            term.elements.map(TermConstraint.Companion::unifiesWith),
            term.tail != null
        )
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
            is TypeCheckConstraint -> {
                if (other.type.isInstance(literal)) this else ImpossibleConstraint
            }
            is ListConstraint -> {
                if (other.check(literal)) this else ImpossibleConstraint
            }
            is UnionTermConstraint -> other.and(this)
            is IdentityTermConstraint -> {
                if (literal == other.literal) this else ImpossibleConstraint
            }
        }
    }
}