package com.github.prologdb.runtime.stdlib.essential.string

import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.nativeConversionRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString

internal val BuiltinStringCodes2 = nativeConversionRule<PrologString, PrologList>(
    "string_codes",
    { str -> PrologList(str.characters.map { PrologInteger(it.code.toLong()) }) },
    { list ->
        if (list.tail != null) throw ArgumentError(1 , "must not have a tail")

        val stringCodesTarget = CharArray(list.elements.size)
        list.elements.forEachIndexed { index, listElement ->
            if (listElement is PrologInteger) {
                if (listElement.value >= 0 && listElement.value < UShort.MAX_VALUE.toLong()) {
                    stringCodesTarget[index] = Char(listElement.value.toInt())
                }
                else throw PrologInvocationContractViolationException("Type error: invalid character code ${listElement.value}")
            }
            else throw PrologInvocationContractViolationException("Type Error: expected integer, found ${listElement.prologTypeName}")
        }

        PrologString(stringCodesTarget)
    }
)
