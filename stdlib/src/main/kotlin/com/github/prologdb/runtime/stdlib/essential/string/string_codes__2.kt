package com.github.prologdb.runtime.stdlib.essential.string

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable

@ExperimentalUnsignedTypes
internal val BuiltinStringCodes2 = nativeRule("string_codes", 2) { args, ctxt ->
    fun convertInputAToListOfCodes(): PrologList {
        val inputA = args.getTyped<PrologString>(0)
        return PrologList(inputA.characters.map { PrologInteger(it.toLong()) })
    }

    fun convertInputBToPrologString(): PrologString {
        val inputB = args.getTyped<PrologList>(1)
        if (inputB.tail != null) throw PrologRuntimeException("Type Error: argument 2 to string_codes/2 must not have a tail")

        val stringCodesTarget = CharArray(inputB.elements.size)
        inputB.elements.forEachIndexed { index, listElement ->
            if (listElement is PrologInteger) {
                if (listElement.value >= 0 && listElement.value < UShort.MAX_VALUE.toLong()) {
                    stringCodesTarget[index] = listElement.value.toChar()
                }
                else throw PrologRuntimeException("Type error: invalid character code ${listElement.value}")
            }
            else throw PrologRuntimeException("Type Error: expected integer, found ${listElement.prologTypeName}")
        }

        return PrologString(stringCodesTarget)
    }

    if (args[0] is Variable && args[1] is Variable) {
        throw PrologRuntimeException("Arguments are not sufficiently instantiated")
    }

    return@nativeRule if (args[0] is PrologString) {
        val referenceValueForB = convertInputAToListOfCodes()
        referenceValueForB.unify(args[1], ctxt.randomVariableScope)
    } else {
        val referenceValueForA = convertInputBToPrologString()
        return@nativeRule referenceValueForA.unify(args[0], ctxt.randomVariableScope)
    }
}
