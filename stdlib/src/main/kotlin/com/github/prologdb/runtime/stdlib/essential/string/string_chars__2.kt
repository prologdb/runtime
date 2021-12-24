package com.github.prologdb.runtime.stdlib.essential.string

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable

/**
 * Implements `string_chars/2`, see http://www.swi-prolog.org/pldoc/doc_for?object=string_chars/2
 */
internal val BuiltinStringChars2 = nativeRule("string_chars", 2) { args, ctxt ->
    fun convertInputAToListOfCharacters(): PrologList {
        val inputA = args.getTyped<PrologString>(0)
        return PrologList(inputA.characters.map { Atom(it.toString()) })
    }

    fun convertInputBToPrologString(): PrologString {
        val inputB = args.getTyped<PrologList>(1)
        // single-character atoms to string
        if (inputB.tail != null) throw PrologRuntimeException("Argument 2 to string_chars/2 must not have a tail.")

        val stringCharsTarget = CharArray(inputB.elements.size)
        inputB.elements.forEachIndexed { index, listElement ->
            if (listElement is Atom && listElement.name.length == 1) {
                stringCharsTarget[index] = listElement.name[0]
            }
            else throw PrologRuntimeException("Type Error: expected character, found ${listElement.prologTypeName}")
        }

        return PrologString(stringCharsTarget)
    }

    if (args[0] is Variable && args[1] is Variable) {
        throw PrologRuntimeException("Arguments are not sufficiently instantiated")
    }

    return@nativeRule if (args[0] is PrologString) {
        val referenceValueForB = convertInputAToListOfCharacters()
        referenceValueForB.unify(args[1], ctxt.randomVariableScope)
    } else {
        val referenceValueForA = convertInputBToPrologString()
        referenceValueForA.unify(args[0], ctxt.randomVariableScope)
    }
}
