package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.runtime.builtin.nativeLibrary

val DictLibrary = nativeLibrary("dict") {
    add(IsDictBuiltin)
    add(GetDictBuiltin)
}