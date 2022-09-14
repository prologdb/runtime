package com.github.prologdb.runtime.stdlib.dicts

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.Variable

val BuiltinGetDict3 = nativeRule("get_dict", 3) { args, ctxt ->
    val keyArg = args[0]
    val dictArg = args.getTyped<PrologDictionary>(1)
    val valueArg = args[2]

    if (keyArg !is Atom && keyArg !is Variable) {
        throw ArgumentTypeError(0, keyArg, Atom::class.java, Variable::class.java)
    }

    if (keyArg is Variable) {
        return@nativeRule yieldAllFinal(LazySequence.ofIterable(dictArg.pairs.entries, principal).mapRemainingNotNull { (dictKey, dictValue) ->
            val valueUnification = valueArg.unify(dictValue, ctxt.randomVariableScope)
            if (valueUnification != null) {
                if (valueUnification.isInstantiated(keyArg)) {
                    if (valueUnification[keyArg] == dictKey) {
                        return@mapRemainingNotNull valueUnification
                    }
                } else {
                    return@mapRemainingNotNull valueUnification.createMutableCopy().apply {
                        instantiate(keyArg, dictKey)
                    }
                }
            }

            return@mapRemainingNotNull null
        })
    } else {
        keyArg as Atom
        val valueForArg = dictArg.pairs[keyArg]

        return@nativeRule if (valueForArg != null) {
            valueArg.unify(valueForArg, ctxt.randomVariableScope)
        } else {
            null
        }
    }
}
