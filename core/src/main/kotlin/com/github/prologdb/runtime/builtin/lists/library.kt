package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.knowledge.library.*

internal val ListsLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(AppendBuiltin)
        add(MemberBuiltin)
        add(LengthBuiltin)
        add(SortBuiltin)
        add(Set2Builtin)
        add(Set3Builtin)
    }

    private fun add(elements: Collection<LibraryEntry>) {
        for (element in elements) {
            add(element)
        }
    }
}