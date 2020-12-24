package com.github.prologdb.runtime.builtin.sort

import com.github.prologdb.runtime.builtin.nativeModule

val SortModule = nativeModule("sort") {
    add(BuiltinPredsort3)
}
