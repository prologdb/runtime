package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.runtime.ClauseIndicator

/**
 * An authorization determines which actions a query can do within
 * a knowledge base.
 */
interface Authorization {
    /**
     * @param clause the clause that is to be accessed
     * @return whether reading facts and rules of that clause is allowed.
     */
    fun mayRead(clause: ClauseIndicator): Boolean

    /**
     * @param clause the clause to be added.
     * @return whether the clause may be added.
     */
    fun mayWrite(clause: ClauseIndicator): Boolean
}

/**
 * Allows all read&write actions.
 */
object ReadWriteAuthorization : Authorization {
    override fun mayRead(clause: ClauseIndicator) = true

    override fun mayWrite(clause: ClauseIndicator) = true
}

object ReadOnlyAuthorization : Authorization {
    override fun mayRead(clause: ClauseIndicator) = true

    override fun mayWrite(clause: ClauseIndicator) = false
}

/**
 * Allows no action.
 */
object NoopAuthorization : Authorization {
    override fun mayRead(clause: ClauseIndicator) = false

    override fun mayWrite(clause: ClauseIndicator) = false
}