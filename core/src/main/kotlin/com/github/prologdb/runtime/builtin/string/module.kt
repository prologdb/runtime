package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.builtin.nativeModule

val StringsModule = nativeModule("strings") {
    add(BuiltinAtomString)
    add(BuiltinStringChars)
    add(BuiltinStringCodes2)
}
