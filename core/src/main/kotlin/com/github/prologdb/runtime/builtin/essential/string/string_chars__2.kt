package com.github.prologdb.runtime.builtin.essential.string

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable

/**
 * Implements `string_chars/2`, see http://www.swi-prolog.org/pldoc/doc_for?object=string_chars/2
 */
internal val BuiltinStringChars = nativeRule("string_chars", 2) { args, ctxt ->
    val inputA = args[0]
    val inputB = args[1]

    fun convertInputAToListOfCharacters(): PrologList {
        if (inputA !is PrologString) throw PrologRuntimeException("Type Error: expected string as first argument to string_chars/2, got ${inputA.prologTypeName}")
        return PrologList(inputA.characters.map { Atom(it.toString()) })
    }

    fun convertInputBToPrologString(): PrologString {
        // single-character atoms to string
        if (inputB !is PrologList) throw PrologRuntimeException("Type Error: expected list as second argument to string_chars/2, got ${inputB.prologTypeName}")
        if (inputB.tail != null) throw PrologRuntimeException("Type Error: expected list as second argument to string_chars/2, got compound")

        val stringCharsTarget = CharArray(inputB.elements.size)
        inputB.elements.forEachIndexed { index, listElement ->
            if (listElement is Atom && listElement.name.length == 1) {
                stringCharsTarget[index] = listElement.name[0]
            }
            else throw PrologRuntimeException("Type Error: expected character, found ${listElement.prologTypeName}")
        }

        return PrologString(stringCharsTarget)
    }

    if (inputA is Variable && inputB is Variable) {
        throw PrologRuntimeException("Arguments are not sufficiently instantiated")
    }

    return@nativeRule if (inputA is PrologString) {
        val referenceValueForB = convertInputAToListOfCharacters()
        referenceValueForB.unify(inputB, ctxt.randomVariableScope)
    }
    else {
        val referenceValueForA = convertInputBToPrologString()
        referenceValueForA.unify(inputA, ctxt.randomVariableScope)
    }
}
