package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

/**
 * An interface to a knowledge base. This is intentionally very simplistic: it can be used
 * both for a locally, in-memory prolog system as well as the main interface for a prologdb
 * driver.
 */
interface KnowledgeBase {
    /**
     * All operators known in this knowledge base. This is exposed mainly to be used in the parser.
     */
    val operators: OperatorRegistry

    /**
     * Starts a proof-search as the given principal.
     */
    fun fulfill(query: Query, randomVariableScope: RandomVariableScope = RandomVariableScope()): LazySequence<Unification>

    /**
     * Invokes the directive with the given name using the given arguments.
     */
    fun invokeDirective(name: String, arguments: Array<out Term>): LazySequence<Unification>
}