package com.github.prologdb.runtime.builtin.dict

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativePredicate
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.Variable

internal val GetDictBuiltin = nativePredicate("get_dict", 3) { args, ctxt ->
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
        for ((dictKey, dictValue) in dictArg.pairs) {
            val valueUnification = valueArg.unify(dictValue, ctxt.randomVariableScope)
            if (valueUnification != null) {
                if (valueUnification.variableValues.isInstantiated(keyArg)) {
                    if (valueUnification.variableValues[keyArg] == dictKey) {
                        yield(valueUnification)
                    }
                }
                else {
                    valueUnification.variableValues.instantiate(keyArg, dictKey)
                    yield(valueUnification)
                }
            }
        }
    }
    else
    {
        keyArg as Atom
        val valueForArg = dictArg.pairs[keyArg]

        if (valueForArg != null) {
            val unification = valueArg.unify(valueForArg, ctxt.randomVariableScope)
            if (unification != null) yield(unification)
        }
    }
}
