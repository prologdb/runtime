package com.github.prologdb.runtime.builtin.essential.string

import com.github.prologdb.runtime.builtin.nativeModule

val EssentialStringsModule = nativeModule("\$strings") {
    add(BuiltinAtomString)
    add(BuiltinStringChars)
    add(BuiltinStringCodes2)
}
