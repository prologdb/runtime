package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.runtime.builtin.nativeModule

val DictModule = nativeModule("dict") {
    add(IsDictBuiltin)
    add(GetDictBuiltin)
}
