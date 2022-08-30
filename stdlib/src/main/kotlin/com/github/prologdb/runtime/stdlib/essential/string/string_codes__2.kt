package com.github.prologdb.runtime.stdlib.essential.string

import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.nativeConversionRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.asIntegerInRange

val BuiltinStringCodes2 = nativeConversionRule<PrologString, PrologList>(
    "string_codes",
    { str -> PrologList(str.characters.map { PrologNumber(it.code.toLong()) }) },
    { list ->
        if (list.tail != null) throw ArgumentError(1 , "must not have a tail")

        val stringCodesTarget = CharArray(list.elements.size)
        list.elements.forEachIndexed { index, listElement ->
            val code = listElement.asIntegerInRange(0L..UShort.MAX_VALUE.toLong())
                ?: throw PrologInvocationContractViolationException("Type error: invalid character code; expected integer in range [0; ${UShort.MAX_VALUE}]")

            stringCodesTarget[index] = Char(code.toInt())
        }

        PrologString(stringCodesTarget)
    }
)
