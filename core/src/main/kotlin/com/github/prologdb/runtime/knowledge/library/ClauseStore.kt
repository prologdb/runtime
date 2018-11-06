package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

interface ClauseStore {
    /**
     * All clauses defined in this store. Used mainly for `listing/0`.
     */
    val clauses: Iterable<Clause>

    /**
     * Finds entries within [exports] that possibly unify with the given [Predicate] (facts or rule heads). This
     * method may just return [exports] but may also implement sophisticated indexing or involve a database engine.
     *
     * The default implementation of this method uses the kotlin stdlib [filter] method.
     */
    fun findFor(predicate: Predicate): Iterable<Clause> = exports.filter { it.arity == predicate.arity && it.name == predicate.name }
}

interface MutableClauseStore : ClauseStore {
    /**
     * Adds the given entry to the library; acts like `assertz/1`: adds the entry to the **end** of the knowledge base.
     */
    fun add(entry: Clause)

    /**
     * Includes all of the exports of the given library into this library.
     *
     * This is defined as a separate method to allow indexing strategies to be hidden
     * and reused. The default implementation of this method simply does `other.exports.forEach(this::add)`.
     */
    fun include(other: ClauseStore) {
        other.exports.forEach(this::add)
    }

    /**
     * Removes facts that unify with the given [Predicate] from the entry store.
     *
     * *Does not affect rules.*.
     *
     * @return Unifications of the given [Predicate] with the removed facts. Every invocation of [LazySequence.tryAdvance]
     *         attempts to remove another entry from the store.
     */
    fun retractFact(fact: Predicate): LazySequence<Unification>

    /**
     * Removes facts (which unify with the given [Predicate]) **or rules (whichs heads unify with
     * the given predicate)** from this store. Acts like `retract/1`.
     *
     * @return The unifications of the given [Predicate] with the removed facts resp. rule heads. Every invocation
     *         of [LazySequence.tryAdvance] attempts to remove another entry from the store.
     */
    fun retract(unifiesWith: Predicate): LazySequence<Unification>

    /**
     * Removes all facts that unify with the given [Predicate] from the entry store. **Does not affect rules**.
     */
    fun retractAllFacts(fact: Predicate) {
        retractFact(fact).consumeAll()
    }

    /**
     * Removes all facts (which unify with the given [Predicate]) **and rules (whichs heads unify with the given
     * [Predicate])** from this store. Acts like `retractAll/1`.
     */
    fun retractAll(unifiesWith: Predicate) {
        retract(unifiesWith).consumeAll()
    }

    /**
     * Removes all facts with the given functor and arity. **Does not affect rules.**
     */
    fun abolishFacts(functor: String, arity: Int) {
        retractAllFacts(Predicate(functor, Array<Term>(arity, { Variable.ANONYMOUS })))
    }

    /**
     * Removes all facts **and rules** with the given functor and arity. Acts like `abolish/1` and `abolish/2`.
     */
    fun abolish(functor: String, arity: Int) {
        retractAll(Predicate(functor, Array<Term>(arity, { Variable.ANONYMOUS })))
    }
}