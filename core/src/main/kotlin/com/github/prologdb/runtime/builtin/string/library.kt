package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.builtin.nativeLibrary
import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedClauseStore
import com.github.prologdb.runtime.knowledge.library.Library

val StringsLibrary = nativeLibrary("strings") {
    add(BuiltinAtomString) // atom_string/2
    add(BuiltinStringChars) // string_chars/2
}