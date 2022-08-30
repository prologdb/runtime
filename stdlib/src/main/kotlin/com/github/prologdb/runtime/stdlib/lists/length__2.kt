package com.github.prologdb.runtime.stdlib.lists

import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.asIntegerInRange

/**
 * length(++List, :Length)
 */
val BuiltinLength2 = nativeRule("length", 2) { args, ctxt ->
    val arg0 = args[0]
    val arg1 = args[1]

    when (arg0) {
        is PrologList -> {
            if (arg0.tail != null) {
                if (arg1 is PrologNumber) {
                    val lengthInput = arg1.asIntegerInRange(0..Int.MAX_VALUE.toLong())
                        ?: throw ArgumentError(1, "Must be an integer in range [0; ${Int.MAX_VALUE}], got $arg1")

                    return@nativeRule arg0.tail!!.unify(listOfLength(lengthInput.toInt(), ctxt), ctxt.randomVariableScope)
                } else if (arg1 is Variable) {
                    val baseLength = arg0.elements.size
                    var tailLength = 0
                    while (true) {
                        val result = arg0.tail!!.unify(listOfLength(tailLength, ctxt), ctxt.randomVariableScope)
                        result.variableValues.instantiate(arg1,
                            PrologNumber((baseLength + tailLength).toLong()))
                        yield(result)
                        tailLength++
                    }
                    @Suppress("UNREACHABLE_CODE")
                    null
                } else {
                    throw ArgumentTypeError(1, arg1, PrologNumber::class.java, Variable::class.java)
                }
            } else {
                val length = PrologNumber(arg0.elements.size.toLong())
                return@nativeRule length.unify(arg1, ctxt.randomVariableScope)
            }
        }
        is Variable -> {
            when (arg1) {
                is Variable -> {
                    var length = 0
                    while (true) {
                        val result = arg0.unify(listOfLength(length, ctxt), ctxt.randomVariableScope)
                        result.variableValues.instantiate(arg1, PrologNumber(length.toLong()))
                        yield(result)
                        length++
                    }
                    @Suppress("UNREACHABLE_CODE")
                    null
                }
                is PrologNumber -> {
                    val lengthInput = arg1.asIntegerInRange(0..Int.MAX_VALUE.toLong())
                        ?: throw ArgumentError(1, "Must be an integer in range [0; ${Int.MAX_VALUE}], got $arg1")
                    return@nativeRule listOfLength(lengthInput.toInt(), ctxt).unify(arg0, ctxt.randomVariableScope)
                }
                else -> {
                    throw PrologInvocationContractViolationException("If argument 1 is a variable, argument 2 must be an integer or a variable (got ${arg1.prologTypeName})")
                }
            }
        }
        else -> throw ArgumentTypeError(0, arg0, PrologList::class.java, Variable::class.java)
    }
}

private fun listOfLength(length: Int, ctxt: ProofSearchContext): PrologList {
    return PrologList(
        List(length) { ctxt.randomVariableScope.createNewRandomVariable() },
        null
    )
}
