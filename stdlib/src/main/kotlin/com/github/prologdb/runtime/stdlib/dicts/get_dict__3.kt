package com.github.prologdb.runtime.stdlib.dicts

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

val BuiltinGetDict3 = nativeRule("get_dict", 3) { args, ctxt ->
    val keyArg = args[0]
    val dictArg = args.getTyped<PrologDictionary>(1)
    val valueArg = args[2]

    if (keyArg !is Atom && keyArg !is Variable) {
        throw ArgumentTypeError(0, keyArg, Atom::class.java, Variable::class.java)
    }

    if (keyArg is Variable) {
        return@nativeRule yieldAllFinal(LazySequence.ofIterable(dictArg.pairs.entries, principal).mapRemainingNotNull { (dictKey, dictValue) ->
            valueArg.unify(dictValue, ctxt.randomVariableScope)
                ?.combinedWith(Unification.of(keyArg, dictKey), ctxt.randomVariableScope)
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
