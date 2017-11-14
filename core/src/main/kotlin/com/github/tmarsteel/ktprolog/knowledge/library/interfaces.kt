package com.github.tmarsteel.ktprolog.knowledge.library

import com.github.tmarsteel.ktprolog.term.Predicate

/**
 * A library as read from a source file.
 */
interface Library {
    /**
     * The elements of the library or knowledge base. The order enforced by the [Iterator]
     * defines the order in proof search will consider the entries.
     */
    val exports: Iterable<LibraryEntry>

    /**
     * Operators defined in this library.
     */
    val operators: OperatorRegistry

    /**
     * Finds entries within [exports] that possibly unify with the given [Predicate] (facts or rule heads). This
     * method may just return [exports] but may also implement sophisticated indexing or involve a database engine.
     *
     * The default implementation of this method uses the kotlin stdlib [filter] method.
     */
    fun findFor(predicate: Predicate): Iterable<LibraryEntry> = exports.filter { it.arity == predicate.arity && it.name == predicate.name }
}

/**
 * A single entry in a knowledge base or library, e.g. a single fact or a single rule.
 */
interface LibraryEntry {
    val name: String
    val arity: Int
}

/**
 * A library that can be modified
 */
interface MutableLibrary : Library {

    override val operators: MutableOperatorRegistry

    /**
     * Adds the given entry to the library
     */
    fun add(entry: LibraryEntry)

    /**
     * Includes all of the exports of the given library into this library.
     *
     * This is defined as a separate method to allow indexing strategies to be hidden
     * and reused. The default implementation of this method simply does `otherLibrary.exports.forEach(this::add)`.
     */
    fun include(otherLibrary: Library) {
        otherLibrary.exports.forEach(this::add)
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
    fun findPrefixOperators(name: String): Set<OperatorDefinition>
    fun findInfixOperators(name: String): Set<OperatorDefinition>
    fun findPostfixOperators(name: String): Set<OperatorDefinition>

    /**
     * Is supposed to display listings and merge multiple operator registries. Should be computed on demand only.
     */
    val allOperators: Iterable<OperatorDefinition>
}

interface MutableOperatorRegistry : OperatorRegistry {
    fun defineOperator(definition: OperatorDefinition)
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
)

enum class OperatorType(val arity: Int) {
    FX(1),
    FY(1),
    XFX(2),
    XFY(2),
    YFX(2),
    XF(1),
    YF(1)
}