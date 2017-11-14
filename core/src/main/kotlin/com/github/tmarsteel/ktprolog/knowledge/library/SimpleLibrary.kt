package com.github.tmarsteel.ktprolog.knowledge.library

import com.github.tmarsteel.ktprolog.knowledge.DefaultOperatorRegistry

/**
 * The most simple implementation of [MutableLibrary] possible: is
 * based on a plain [MutableList] and uses the default implementations
 * declared in [Library] and [MutableLibrary]
 */
class SimpleLibrary(givenEntries: Iterable<LibraryEntry> = emptyList()) : MutableLibrary {
    private val entries = ArrayList(givenEntries.toList())

    override val exports = entries

    override val operators = DefaultOperatorRegistry()

    override fun add(entry: LibraryEntry) {
        entries.add(entry)
    }
}