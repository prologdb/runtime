package com.github.prologdb.parser

import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term

fun <T : Term> T.withMockSourceLocation(): T {
    @Suppress("UNCHECKED_CAST")
    val nestedMapped : T = when(this) {
        is CompoundTerm     -> CompoundTerm(functor, arguments.map { it.withMockSourceLocation() }.toTypedArray()) as T
        is PrologList       -> PrologList(elements.map { it.withMockSourceLocation() }, tail) as T
        is PrologDictionary -> PrologDictionary(pairs.map { (key, value) ->
            Pair(key.withMockSourceLocation(), value.withMockSourceLocation())
        }.toMap(), tail) as T
        else                -> this
    }

    nestedMapped.sourceInformation = SourceLocationRange(
        SourceLocation.EOF,
        SourceLocation.EOF
    )
    return nestedMapped
}