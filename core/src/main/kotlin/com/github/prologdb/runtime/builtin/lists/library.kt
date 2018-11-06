package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.knowledge.library.Clause
import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedClauseStore
import com.github.prologdb.runtime.knowledge.library.Library

internal val ListsLibrary : Library = object : SimpleLibrary(DoublyIndexedClauseStore(), DefaultOperatorRegistry()) {
    init {
        add(AppendBuiltin)
        add(MemberBuiltin)
        add(LengthBuiltin)
        add(SortBuiltin)
        add(Set2Builtin)
        add(Set3Builtin)
    }

    private fun add(elements: Collection<Clause>) {
        for (element in elements) {
            add(element)
        }
    }
}