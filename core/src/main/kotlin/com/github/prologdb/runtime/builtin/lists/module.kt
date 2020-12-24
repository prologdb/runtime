package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.nativeModule

val ListsModule = nativeModule("lists") {
    add(BuiltinLength2)
    add(BuiltinSort2)
    add(BuiltinAppend3)
    add(BuiltinMember2)
    add(BuiltinSet2)
    add(BuiltinSet3)

    add(BuiltinIota3)
    add(BuiltinIota4)

    import("essential", "\$equality")
}
