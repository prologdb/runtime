package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.builtin.nativeLibrary

val StringsLibrary = nativeLibrary("strings") {
    add(BuiltinAtomString) // atom_string/2
    add(BuiltinStringChars) // string_chars/2
}