package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologNumber

/**
 * Takes an arithmetic compound term (e.g. +(1,2) or mod(23,4)) and returns the calculated value
 */
typealias Calculator = (CompoundTerm) -> PrologNumber

/**
 * Keeps track of ways to evaluate mathematical compounds. This is global; registring a new calculator
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
     * Evaluates the given compound term as an arithmetic expression.
     */
    fun evaluate(compoundTerm: CompoundTerm): PrologNumber {
        val calculator = getCalculator(compoundTerm.functor, compoundTerm.arity) ?: throw PrologRuntimeException("Arithmetic operator ${compoundTerm.functor}/${compoundTerm.arity} is not defined")
        return calculator(compoundTerm)
    }

    fun registerOperator(operatorName: String, calculator: (PrologNumber) -> PrologNumber) {
        registerOperator(operatorName, 1..1) { termAST ->
            if (termAST.arity != 1 || termAST.functor != operatorName) throw PrologRuntimeException("Calculator for $operatorName/1 cannot be invoked with an instance of ${termAST.functor}/${termAST.arity}")
            calculator(termAST.arguments[0].asPrologNumber)
        }
    }

    fun registerOperator(operatorName: String, calculator: (PrologNumber, PrologNumber) -> PrologNumber) {
        registerOperator(operatorName, 2..2) { compoundTerm ->
            if (compoundTerm.arity != 2 || compoundTerm.functor != operatorName) throw PrologRuntimeException("Calculator for $operatorName/2 cannot be invoked with an instance of ${compoundTerm.functor}/${compoundTerm.arity}")
            calculator(compoundTerm.arguments[0].asPrologNumber, compoundTerm.arguments[1].asPrologNumber)
        }
    }

    init {
        // common binary operators
        registerOperator("+", PrologNumber::plus)
        registerOperator("-", PrologNumber::minus)
        registerOperator("*", PrologNumber::times)
        registerOperator("/", PrologNumber::div)
        registerOperator("mod", PrologNumber::rem)
        registerOperator("^", PrologNumber::toThe)

        registerOperator("+", PrologNumber::unaryPlus)
        registerOperator("-", PrologNumber::unaryMinus)
    }
}