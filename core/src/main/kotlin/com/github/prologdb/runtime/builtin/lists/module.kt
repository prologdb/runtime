package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.nativeModule

val ListsModule = nativeModule("lists") {
    add(AppendBuiltin)
    add(MemberBuiltin)
    add(LengthBuiltin)
    add(SortBuiltin)
    add(Set2Builtin)
    add(Set3Builtin)

    add(BuiltinIota3)
    add(BuiltinIota4)
}
