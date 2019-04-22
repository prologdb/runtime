package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.builtin.nativeModule

val MathModule = nativeModule("math") {
    add(BuiltinIs)
    add(BuiltinGreaterThan)
    add(BuiltinGreaterThanOrEqual)
    add(BuiltinLessThan)
    add(BuiltinLessThanOrEqual)
}
