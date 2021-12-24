package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable

/**
 * length(++List, :Length)
 */
internal val BuiltinLength2 = nativeRule("length", 2) { args, ctxt ->
    val arg0 = args[0]
    val arg1 = args[1]

    when (arg0) {
        is PrologList -> {
            if (arg0.tail != null) {
                when (arg1) {
                    is PrologInteger -> {
                        if (arg1.value < 0) {
                            throw PrologRuntimeException("Argument 2 to length/2 must not be negative")
                        }
                        return@nativeRule arg0.tail!!.unify(listOfLength(arg1.value.toInt(), ctxt), ctxt.randomVariableScope)
                    }
                    is Variable -> {
                        val baseLength = arg0.elements.size
                        var tailLength = 0
                        while (true) {
                            val result = arg0.tail!!.unify(listOfLength(tailLength, ctxt), ctxt.randomVariableScope)
                            result.variableValues.instantiate(arg1,
                                PrologInteger.createUsingStringOptimizerCache((baseLength + tailLength).toLong()))
                            yield(result)
                            tailLength++
                        }
                        @Suppress("UNREACHABLE_CODE")
                        null
                    }
                    else -> throw ArgumentTypeError(args.indicator, 1, arg1, PrologInteger::class.java, Variable::class.java)
                }
            } else {
                val length = PrologInteger.createUsingStringOptimizerCache(arg0.elements.size.toLong())
                return@nativeRule length.unify(arg1, ctxt.randomVariableScope)
            }
        }
        is Variable -> {
            when (arg1) {
                is Variable -> {
                    var length = 0
                    while (true) {
                        val result = arg0.unify(listOfLength(length, ctxt), ctxt.randomVariableScope)
                        result.variableValues.instantiate(arg1, PrologInteger.createUsingStringOptimizerCache(length.toLong()))
                        yield(result)
                        length++
                    }
                    @Suppress("UNREACHABLE_CODE")
                    null
                }
                is PrologInteger -> {
                    return@nativeRule listOfLength(arg1.value.toInt(), ctxt).unify(arg0, ctxt.randomVariableScope)
                }
                else -> {
                    throw PrologRuntimeException("If argument 1 to length/2 is a variable, argument 2 must be an integer or a variable (got ${arg1.prologTypeName})")
                }
            }
        }
        else -> throw ArgumentTypeError(args.indicator, 0, arg0, PrologList::class.java, Variable::class.java)
    }
}

private fun listOfLength(length: Int, ctxt: ProofSearchContext): PrologList {
    return PrologList(
        List(length) { ctxt.randomVariableScope.createNewRandomVariable() },
        null
    )
}
