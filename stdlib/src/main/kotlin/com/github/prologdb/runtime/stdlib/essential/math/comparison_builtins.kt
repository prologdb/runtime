package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.stdlib.nativeRule

val BuiltinGreaterThan2 = numericComparisonBuiltin(">") { it > 0 }
val BuiltinGreaterThanOrEqual2 = numericComparisonBuiltin(">=") { it >= 0 }
val BuiltinLessThan2 = numericComparisonBuiltin("<") { it < 0 }
val BuiltinLessThanOrEqual2 = numericComparisonBuiltin("=<") { it <= 0 }
val BuiltinNumericNotEqual2 = numericComparisonBuiltin("=\\=") { it != 0 }

private inline fun numericComparisonBuiltin(name: String, crossinline tester: (comparisonResult: Int) -> Boolean) = nativeRule(name, 2) { args, ctxt ->
    val a = args[0].evaluateAsMathematicalExpression(ctxt.mathContext)
    val b = args[1].evaluateAsMathematicalExpression(ctxt.mathContext)
    val comparisonResult = a.compareTo(b)
    Unification.whether(tester(comparisonResult))
}