package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.runtime.builtin.nativeModule

val DictLibrary = nativeModule("dict") {
    add(IsDictBuiltin)
    add(GetDictBuiltin)
}
