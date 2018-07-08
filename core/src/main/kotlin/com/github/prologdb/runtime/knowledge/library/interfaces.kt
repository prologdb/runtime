package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

/**
 * A indicator of a predicate, e.g. `likes/2`.
 */
interface PredicateIndicator {
    val name: String
    val arity: Int

    /** The prolog-idiomatic representation of this indicator, e.g. for `name=foobar, arity=2` is `'/'(foobar, 2)` */
    val idiomaticIndicator: Predicate
        get() = object : Predicate("/", arrayOf(Atom(name), com.github.prologdb.runtime.term.PrologInteger(arity.toLong()))) {
            override fun toString(): String = "${this@PredicateIndicator.name}/${this@PredicateIndicator.arity}"
        }
}

/**
 * A single entry in a knowledge base or library, e.g. a single fact or a single rule.
 * @see Predicate
 * @see com.github.prologdb.runtime.knowledge.Rule
 */
interface LibraryEntry : PredicateIndicator {
    /**
     * Unifies the given predicate (`other`) with this entry; if this is a fact (a [Predicate]), unifies with
     * the given predicate and ignores the given [KnowledgeBase]. If this is a rule, uses the [KnowledgeBase]
     * to run the query (in case the head and the given [Predicate] unify).
     */
    fun unifyWithKnowledge(other: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope) : LazySequence<Unification>
}

interface LibraryEntryStore {
    /**
     * The elements of the library or knowledge base. The order enforced by the [Iterator]
     * defines the order in proof search will consider the entries.
     */
    val exports: Iterable<LibraryEntry>

    /**
     * Finds entries within [exports] that possibly unify with the given [Predicate] (facts or rule heads). This
     * method may just return [exports] but may also implement sophisticated indexing or involve a database engine.
     *
     * The default implementation of this method uses the kotlin stdlib [filter] method.
     */
    fun findFor(predicate: Predicate): Iterable<LibraryEntry> = exports.filter { it.arity == predicate.arity && it.name == predicate.name }
}

interface MutableLibraryEntryStore : LibraryEntryStore {
    /**
     * Adds the given entry to the library; acts like `assertz/1`: adds the entry to the **end** of the knowledge base.
     */
    fun add(entry: LibraryEntry)

    /**
     * Includes all of the exports of the given library into this library.
     *
     * This is defined as a separate method to allow indexing strategies to be hidden
     * and reused. The default implementation of this method simply does `other.exports.forEach(this::add)`.
     */
    fun include(other: LibraryEntryStore) {
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

/**
 * Holds operators defined in a knowledge base. This might seem like a concern of the parser since, at the core level,
 * it does not make a difference whether `+(1,2)` was parsed from `1 + 2` or from `+(1,2)`. But operator definitions
 * are at the very heart of the language (e.g. =/2). It would be possible to define the core without the syntax sugar
 * but that would make it a real chore to go from core + parser to a working, compliant REPL. The core concerns itself
 * with operator definitions because it makes the library easier to use.
 */
interface OperatorRegistry {
    /**
     * Returns all definitions for operators with the given name
     */
    fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition>

    /**
     * Is supposed to be used to display listings and merge multiple operator registries.
     * Should be computed on demand only.
     */
    val allOperators: Iterable<OperatorDefinition>
}

interface MutableOperatorRegistry : OperatorRegistry {
    fun defineOperator(definition: OperatorDefinition)

    /**
     * Adds all the operators defined in `other` to this registry. The default implementation simply does something
     * equivalent to `other.allOperators.forEach(this::defineOperator)`
     */
    fun include(other: OperatorRegistry) {
        other.allOperators.forEach(this::defineOperator)
    }
}

/**
 * Defines an operator to use in a prolog program.
 */
data class OperatorDefinition (
    /**
     * The precedence, between 0 and 1200.
     */
    val precedence: Short,

    /**
     * The type of this operator ; defines how it relates towards its surroundings
     */
    val type: OperatorType,

    /**
     * The name of the operator
     */
    val name: String
) {
    override fun toString() = "op($precedence, ${type.name.toLowerCase()}, $name)"
}

enum class OperatorType(val arity: Int) {
    FX(1),
    FY(1),
    XFX(2),
    XFY(2),
    YFX(2),
    XF(1),
    YF(1);

    val isPrefix by lazy { this == FX || this == FY }
    val isInfix by lazy { this == XFX || this == XFY || this == YFX }
    val isPostfix by lazy { this == XF || this == YF }
}

/**
 * A library as read from a source file.
 */
interface Library : LibraryEntryStore, OperatorRegistry

/**
 * A library that can be modified
 */
interface MutableLibrary : Library, MutableLibraryEntryStore, MutableOperatorRegistry {
    /**
     * Includes all exports (facts, rules and operators) of the given library into this library.
     *
     * The default implementation simply delegates to [MutableLibraryEntryStore.include] and [MutableOperatorRegistry.include]
     */
    fun include(otherLibrary: Library) {
        this.include(otherLibrary as LibraryEntryStore)
        this.include(otherLibrary as OperatorRegistry)
    }
}