package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

/**
 * Implements the is/2 builtin that evaluates mathematical expressions
 */
val BuiltinIs2 = nativeRule("is", 2) { args, ctxt ->
    val inputForA = args[0]
    val inputForB = args[1]

    if (inputForA is Variable) {
        return@nativeRule inputForA.unify(inputForB.evaluateAsMathematicalExpression(ctxt.mathContext), ctxt.randomVariableScope)
    }

    if (inputForB is Variable) {
        return@nativeRule inputForB.unify(inputForA.evaluateAsMathematicalExpression(ctxt.mathContext), ctxt.randomVariableScope)
    }

    return@nativeRule Unification.whether(
        inputForA.evaluateAsMathematicalExpression(ctxt.mathContext) == inputForB.evaluateAsMathematicalExpression(ctxt.mathContext)
    )
}
