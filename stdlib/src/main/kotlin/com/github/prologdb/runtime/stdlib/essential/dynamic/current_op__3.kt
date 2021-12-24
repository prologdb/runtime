package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.unify

val BuiltinCurrentOp3 = nativeRule("current_op", 3) { args, ctxt ->
    val nameArg = args[2]

    val baseOperators = if (nameArg is Atom) {
        ctxt.operators.getOperatorDefinitionsFor(nameArg.name)
    } else {
        ctxt.operators.allOperators
    }

    return@nativeRule yieldAllFinal(LazySequence.ofIterable(baseOperators)
        .mapRemainingNotNull { opDef ->
            arrayOf(
                PrologInteger.createUsingStringOptimizerCache(opDef.precedence.toLong()),
                Atom(opDef.type.name.toLowerCase()),
                Atom(opDef.name)
            ).unify(args.raw, ctxt.randomVariableScope)
        }
    )
}
