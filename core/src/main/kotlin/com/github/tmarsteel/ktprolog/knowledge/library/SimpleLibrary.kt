package com.github.tmarsteel.ktprolog.knowledge.library

/**
 * The most simple implementation of [MutableLibrary] possible: is
 * based on a plain [MutableList] and uses the default implementations
 * declared in [Library] and [MutableLibrary]
 */
class SimpleLibrary(givenEntries: Iterable<LibraryEntry> = emptyList()) : MutableLibrary {
    private val entries = ArrayList(givenEntries.toList())

    override val exports = entries

    override fun add(entry: LibraryEntry) {
        entries.add(entry)
    }
}