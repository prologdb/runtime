package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.runtime.builtin.nativeLibrary
import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedClauseStore
import com.github.prologdb.runtime.knowledge.library.Library

val DictLibrary = nativeLibrary("dict") {
    add(IsDictBuiltin)
    add(GetDictBuiltin)
}