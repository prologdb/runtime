package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.builtin.nativeModule

val StringsLibrary = nativeModule("strings") {
    add(BuiltinAtomString) // atom_string/2
    add(BuiltinStringChars) // string_chars/2
}
