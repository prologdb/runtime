package com.github.tmarsteel.ktprolog.knowledge.library

/**
 * The most simple implementation of [MutableLibraryEntryStore] possible: is
 * based on a plain [MutableList] and uses the default implementations
 * declared in [LibraryEntryStore] and [MutableLibraryEntryStore]
 */
class SimpleLibraryEntryStore(givenEntries: Iterable<LibraryEntry> = emptyList()) : MutableLibraryEntryStore {
    private val entries = ArrayList(givenEntries.toList())

    override val exports = entries

    override fun add(entry: LibraryEntry) {
        entries.add(entry)
    }
}