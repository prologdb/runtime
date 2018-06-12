package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.term.Number
import com.github.prologdb.runtime.term.Predicate

/**
 * Takes an arithmetic predicate (e.g. +(1,2) or mod(23,4)) and returns the calculated value
 */
typealias Calculator = (Predicate) -> Number

/**
 * Keeps track of ways to evaluate mathematical predicates. This is global; registring a new calculator
 * will affect all prolog runtimes.
 *
 * **This is not an [OperatorRegistry]!**
 */
object MathOperatorRegistry {
    /**
     * Maps operator names to calculators
     */
    private val calculators: MutableMap<String, ArityMap<Calculator>> = mutableMapOf()

    fun registerOperator(operatorName: String, arities: IntRange, calculator: Calculator) {
        if (arities.first <= 0) {
            throw IllegalArgumentException("Cannot register an arithmetic operator with arity less than 1")
        }

        val arityMap: ArityMap<Calculator>
        if (operatorName in calculators) {
            arityMap = calculators[operatorName]!!
        } else {
            arityMap = ArityMap()
            calculators[operatorName] = arityMap
        }

        for (arity in arities) {
            arityMap[arity] = calculator
        }
    }

    fun getCalculator(operatorName: String, arity: Int): Calculator? = calculators[operatorName]?.get(arity)

    /**
     * Evaluates the given predicate as an arithmetic expression.
     */
    fun evaluate(predicate: Predicate): Number {
        val calculator = getCalculator(predicate.name, predicate.arity) ?: throw PrologRuntimeException("Arithmetic operator ${predicate.name}/${predicate.arity} is not defined")
        return calculator(predicate)
    }

    fun registerOperator(operatorName: String, calculator: (Number) -> Number) {
        registerOperator(operatorName, 1..1, { predicate ->
            if (predicate.arity != 1 || predicate.name != operatorName) throw PrologRuntimeException("Calculator for $operatorName/1 cannot be invoked with an instance of ${predicate.name}/${predicate.arity}")
            calculator(predicate.arguments[0].asNumber)
        })
    }

    fun registerOperator(operatorName: String, calculator: (Number, Number) -> Number) {
        registerOperator(operatorName, 2..2, { predicate ->
            if (predicate.arity != 2 || predicate.name != operatorName) throw PrologRuntimeException("Calculator for $operatorName/2 cannot be invoked with an instance of ${predicate.name}/${predicate.arity}")
            calculator(predicate.arguments[0].asNumber, predicate.arguments[1].asNumber)
        })
    }

    init {
        // common binary operators
        registerOperator("+", Number::plus)
        registerOperator("-", Number::minus)
        registerOperator("*", Number::times)
        registerOperator("/", Number::div)
        registerOperator("mod", Number::rem)
        registerOperator("^", Number::toThe)

        registerOperator("+", Number::unaryPlus)
        registerOperator("-", Number::unaryMinus)
    }
}