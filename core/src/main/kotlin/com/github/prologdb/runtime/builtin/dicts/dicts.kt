package com.github.prologdb.runtime.builtin.dicts

import com.github.prologdb.runtime.builtin.nativeModule

val DictsModule = nativeModule("dicts") {
    add(BuiltinGetDict3)
}
