package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.knowledge.library.*

val ListsLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(AppendBuiltin)
    }

    private fun add(elements: Collection<LibraryEntry>) {
        elements.forEach(::add)
    }
}