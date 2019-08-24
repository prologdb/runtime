package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
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
            is UnionTermConstraint -> other.and(this)
        }
    }
}

data class ListConstraint(
    /**
     * The list must have at least as many elements.
     */
    val nEntries: Int,

    /**
     * Whether more entries than [nEntries] are allowed.
     */
    val moreAllowed: Boolean
) : TermConstraint() {

    init {
        require(nEntries >= 0)
    }

    override fun check(term: Term): Boolean {
        if (term !is PrologList) return false

        if (term.elements.size < nEntries) return false
        if (!moreAllowed && term.elements.size > nEntries) return false

        return true
    }

    override fun and(other: TermConstraint): TermConstraint = when(other) {
        is NoopConstraint -> this
        is ImpossibleConstraint -> other
        is ListConstraint ->
            if (this == other) this
            else if (this.moreAllowed && other.moreAllowed) {
                if (this.nEntries == other.nEntries) {
                    this
                } else {
                    ListConstraint(max(this.nEntries, other.nEntries), true)
                }
            }
            else {
                if (this.nEntries <= other.nEntries && this.moreAllowed) {
                    other
                }
                else if (this.nEntries >= other.nEntries && other.moreAllowed) {
                    this
                }
                else {
                    ImpossibleConstraint
                }
            }
        is TypeCheckConstraint,
        is UnionTermConstraint -> other.and(this)
    }

    companion object {
        @JvmStatic
        fun unifiesWith(term: PrologList) = ListConstraint(term.elements.size, term.tail != null)
    }
}