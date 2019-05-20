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
}

/**
 * Allows all read&write actions.
 */
object ReadWriteAuthorization : Authorization {
    override fun mayRead(predicate: FullyQualifiedClauseIndicator) = true

    override fun mayWrite(predicate: FullyQualifiedClauseIndicator) = true
}

object ReadOnlyAuthorization : Authorization {
    override fun mayRead(predicate: FullyQualifiedClauseIndicator) = true

    override fun mayWrite(predicate: FullyQualifiedClauseIndicator) = false
}

/**
 * Allows no action.
 */
object NoopAuthorization : Authorization {
    override fun mayRead(predicate: FullyQualifiedClauseIndicator) = false

    override fun mayWrite(predicate: FullyQualifiedClauseIndicator) = false
}