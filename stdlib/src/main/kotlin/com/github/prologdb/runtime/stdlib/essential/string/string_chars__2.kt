package com.github.prologdb.runtime.stdlib.essential.string

import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.nativeConversionRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString

internal val BuiltinStringChars2 = nativeConversionRule<PrologString, PrologList>(
    "string_chars",
    { str -> PrologList(str.characters.map { Atom(it.toString()) }) },
    { list ->
        if (list.tail != null) throw ArgumentError(1, "must not have a tail.")

        val stringCharsTarget = CharArray(list.elements.size)
        list.elements.forEachIndexed { index, listElement ->
            if (listElement is Atom && listElement.name.length == 1) {
                stringCharsTarget[index] = listElement.name[0]
            }
            else throw PrologInvocationContractViolationException("Type Error: expected character, found ${listElement.prologTypeName}")
        }

        PrologString(stringCharsTarget)
    }
)