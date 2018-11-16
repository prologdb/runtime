package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.NativeLibraryBuilder
import com.github.prologdb.runtime.builtin.nativeLibrary
import com.github.prologdb.runtime.knowledge.library.Clause

val ListsLibrary = nativeLibrary("lists") {
    add(AppendBuiltin)
    add(MemberBuiltin)
    add(LengthBuiltin)
    add(SortBuiltin)
    add(Set2Builtin)
    add(Set3Builtin)
}

private fun NativeLibraryBuilder.add(elements: Collection<Clause>) {
    for (element in elements) {
        add(element)
    }
}