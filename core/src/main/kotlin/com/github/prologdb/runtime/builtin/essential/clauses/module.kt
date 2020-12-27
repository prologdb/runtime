package com.github.prologdb.runtime.builtin.essential.clauses

import com.github.prologdb.runtime.builtin.nativeModule

val EssentialClausesModule = nativeModule("\$clauses") {
    add(BuiltinAssert1)
    add(BuiltinAbolish1)
}
