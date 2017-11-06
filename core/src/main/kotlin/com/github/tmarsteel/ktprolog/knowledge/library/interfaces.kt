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