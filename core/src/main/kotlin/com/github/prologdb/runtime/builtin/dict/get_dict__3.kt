package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.Variable

internal val GetDictBuiltin = nativeRule("get_dict", 3) { args, ctxt ->
    val keyArg = args[0]
    val dictArg = args[1]
    val valueArg = args[2]

    if (keyArg !is Atom && keyArg !is Variable) {
        throw PrologRuntimeException("Type error: argument 1 to get_dict/3 must be an atom or unbound")
    }

    if (dictArg !is PrologDictionary) {
        if (dictArg is Variable) {
            throw PrologRuntimeException("Argument 2 to get_dict/3 is not sufficiently instantiated")
        } else {
            throw PrologRuntimeException("Type error: argument 2 to get_dict/3 must be a dict, ${dictArg.prologTypeName} given")
        }
    }

    if (keyArg is Variable) {
        return@nativeRule yieldAllFinal(LazySequence.ofIterable(dictArg.pairs.entries, principal).mapRemainingNotNull { (dictKey, dictValue) ->
            val valueUnification = valueArg.unify(dictValue, ctxt.randomVariableScope)
            if (valueUnification != null) {
                if (valueUnification.variableValues.isInstantiated(keyArg)) {
                    if (valueUnification.variableValues[keyArg] == dictKey) {
                        return@mapRemainingNotNull valueUnification
                    }
                } else {
                    valueUnification.variableValues.instantiate(keyArg, dictKey)
                    return@mapRemainingNotNull valueUnification
                }
            }

            return@mapRemainingNotNull null
        })
    }
    else
    {
        keyArg as Atom
        val valueForArg = dictArg.pairs[keyArg]

        return@nativeRule if (valueForArg != null) {
            valueArg.unify(valueForArg, ctxt.randomVariableScope)
        } else {
            null
        }
    }
}
