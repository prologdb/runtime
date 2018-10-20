package com.github.prologdb.runtime.builtin.lists

import com.github.prologdb.runtime.builtin.*
import com.github.prologdb.runtime.knowledge.library.LibraryEntry

/**
 *     length([], 0).
 *     length([_|T], L) :- length(T, TL), L is TL + 1.
 */
internal val LengthBuiltin = listOf<LibraryEntry>(
    "length"(L(), N(0)),
    ("length"(L(V("_")).tail(V("T")), V("L"))) {
        "length"(V("T"), V("TL")) AND "is"(V("L"), "+"(V("TL"), N(1)))
    }
)