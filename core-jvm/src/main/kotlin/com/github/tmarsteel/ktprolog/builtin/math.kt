package com.github.tmarsteel.ktprolog.builtin

import com.github.tmarsteel.ktprolog.PrologRuntimeException
import com.github.tmarsteel.ktprolog.term.Decimal
import com.github.tmarsteel.ktprolog.term.Integer

object BuiltinMathOperators {
    /**
     * Invoked from [MathOperatorRegistry]'s init code
     */
    fun register() {
        MathOperatorRegistry.registerOperator("^", { lhs, rhs -> when(lhs) {
            is Integer -> when(rhs) {
                is Integer -> Integer(Math.pow(lhs.value.toDouble(), rhs.value.toDouble()).toLong())
                is Decimal -> Decimal(Math.pow(lhs.value.toDouble(), rhs.value))
                else -> throw PrologRuntimeException("Unsupported number type detected")
            }
            is Decimal -> when(rhs) {
                is Integer -> Decimal(Math.pow(lhs.value, rhs.value.toDouble()))
                is Decimal -> Decimal(Math.pow(lhs.value, rhs.value))
                else -> throw PrologRuntimeException("Unsupported number type detected")
            }
            else -> throw PrologRuntimeException("Unsupported number type detected")
        }})
    }
}