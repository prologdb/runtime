package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedLibraryEntryStore
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.SimpleLibrary
import com.github.prologdb.runtime.term.Integer as PrologInteger

val StringsLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {
    init {
        add(BuiltinAtomString) // atom_string/2
        add(BuiltinStringChars) // string_chars/2
    }
}