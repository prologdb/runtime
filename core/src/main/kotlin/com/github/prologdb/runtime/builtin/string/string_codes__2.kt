package com.github.prologdb.runtime.builtin.string

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable

@ExperimentalUnsignedTypes
internal val BuiltinStringCodes2 = nativeRule("string_codes", 2) { args, ctxt ->
    val inputA = args[0]
    val inputB = args[1]

    fun convertInputAToListOfCodes(): PrologList {
        if (inputA !is PrologString) throw PrologRuntimeException("Type Error: argument 1 to string_codes/2 must be a string, got ${inputA.prologTypeName}")
        return PrologList(inputA.characters.map { PrologInteger(it.toLong()) })
    }

    fun convertInputBToPrologString(): PrologString {
        // single-character atoms to string
        if (inputB !is PrologList) throw PrologRuntimeException("Type Error: argument 2 to string_codes/2 must be a list, got ${inputB.prologTypeName}")
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

    if (inputA is Variable && inputB is Variable) {
        throw PrologRuntimeException("Arguments are not sufficiently instantiated")
    }

    if (inputA is PrologString) {
        val referenceValueForB = convertInputAToListOfCodes()
        val result = referenceValueForB.unify(inputB, ctxt.randomVariableScope)
        if (result != null) yield(result)
    }
    else {
        val referenceValueForA = convertInputBToPrologString()
        val result = referenceValueForA.unify(inputA, ctxt.randomVariableScope)
        if (result != null) yield(result)
    }
}
