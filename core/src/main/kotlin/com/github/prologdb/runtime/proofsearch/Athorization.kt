package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.runtime.FullyQualifiedClauseIndicator

/**
 * An authorization determines which actions a query can do within
 * a knowledge base.
 */
interface Authorization {
    /**
     * @param clause the clause that is to be accessed
     * @return whether reading facts and rules of that clause is allowed.
     */
    fun mayRead(predicate: FullyQualifiedClauseIndicator): Boolean

    /**
     * @param clause the clause to be added.
     * @return whether the clause may be added.
     */
    fun mayWrite(predicate: FullyQualifiedClauseIndicator): Boolean

    /**
     * @return an authorization that requires both `this` and [other]
     * to approve any action.
     */
    fun restrictWith(other: Authorization): Authorization = when(other) {
        is PermitAllAuthorization -> this
        is DenyAllAuthorization -> DenyAllAuthorization
        else -> CombinedAuthorization(listOf(this, other))
    }
}

/**
 * Allows all read&write actions.
 */
object PermitAllAuthorization : Authorization {
    override fun mayRead(predicate: FullyQualifiedClauseIndicator) = true

    override fun mayWrite(predicate: FullyQualifiedClauseIndicator) = true

    override fun restrictWith(other: Authorization) = other
}

object ReadOnlyAuthorization : Authorization {
    override fun mayRead(predicate: FullyQualifiedClauseIndicator) = true

    override fun mayWrite(predicate: FullyQualifiedClauseIndicator) = false
}

object DenyAllAuthorization : Authorization {
    override fun mayRead(predicate: FullyQualifiedClauseIndicator) = false

    override fun mayWrite(predicate: FullyQualifiedClauseIndicator) = false

    override fun restrictWith(other: Authorization): Authorization = this
}

class CombinedAuthorization(val constituents: Iterable<Authorization>): Authorization {
    override fun mayRead(predicate: FullyQualifiedClauseIndicator): Boolean {
        return constituents.all { it.mayRead(predicate) }
    }

    override fun mayWrite(predicate: FullyQualifiedClauseIndicator): Boolean {
        return constituents.all { it.mayWrite(predicate) }
    }

    override fun restrictWith(other: Authorization): Authorization {
        return if (other is CombinedAuthorization) {
            CombinedAuthorization(constituents + other.constituents)
        } else {
            CombinedAuthorization(constituents + listOf(other))
        }
    }
}