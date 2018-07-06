package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.*
import com.github.prologdb.runtime.knowledge.library.LibraryEntry

internal val LengthBuiltin = listOf<LibraryEntry>(
    "length"(L(), N(0)),
    ("length"(L(V("_")).tail(V("T")), V("L"))) {
        "length"(V("T"), V("TL")) AND "is"(V("L"), "+"(V("TL"), N(1)))
    }
)