package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedLibraryEntryStore
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.SimpleLibrary
import com.github.prologdb.runtime.term.Integer as PrologInteger
import com.github.prologdb.runtime.term.List as PrologList

val StringsLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(AtomStringPredicate) // atom_string/2
        add(StringCharsPredicate) // string_chars/2
    }
}