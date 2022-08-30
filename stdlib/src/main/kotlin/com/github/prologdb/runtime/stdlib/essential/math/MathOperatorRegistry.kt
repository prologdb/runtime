package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.MathContext
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.util.ArityMap

/**
 * Takes an arithmetic compound term (e.g. +(1,2) or mod(23,4)) and returns the calculated value
 */
typealias Calculator = (CompoundTerm, MathContext) -> PrologNumber

/**
 * Keeps track of ways to evaluate mathematical compounds. This is global; registring a new calculator
 * will affect all prolog runtimes.
 *
 * **This is not an [com.github.prologdb.runtime.util.OperatorRegistry]!**
 */
object MathOperatorRegistry {
    /**
     * Maps operator names to calculators
     */
    private val calculators: MutableMap<String, ArityMap<Calculator>> = mutableMapOf()

    fun registerContextualOperator(operatorName: String, arities: IntRange, calculator: Calculator) {
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
    fun evaluate(compoundTerm: CompoundTerm, context: MathContext): PrologNumber {
        val calculator = getCalculator(compoundTerm.functor, compoundTerm.arity)
            ?: throw UndefinedMathOperatorException(ClauseIndicator.of(compoundTerm))
        return calculator(compoundTerm, context)
    }

    fun registerContextualOperator(operatorName: String, calculator: (PrologNumber, MathContext) -> PrologNumber) {
        registerContextualOperator(operatorName, 1..1) { termAST, context ->
            if (termAST.arity != 1 || termAST.functor != operatorName) {
                throw InvalidMathOperatorInvocationException(ClauseIndicator.of(operatorName, 1), ClauseIndicator.of(termAST))
            }
            calculator(termAST.arguments[0].evaluateAsMathematicalExpression(context), context)
        }
    }

    fun registerOperator(operatorName: String, calculator: (PrologNumber) -> PrologNumber) {
        registerContextualOperator(operatorName) { termAST, context ->
            calculator(termAST.evaluateAsMathematicalExpression(context))
        }
    }

    fun registerContextualOperator(operatorName: String, calculator: (PrologNumber, PrologNumber, MathContext) -> PrologNumber) {
        registerContextualOperator(operatorName, 2..2) { compoundTerm, context ->
            if (compoundTerm.arity != 2 || compoundTerm.functor != operatorName) {
                throw InvalidMathOperatorInvocationException(ClauseIndicator.of(operatorName, 2), ClauseIndicator.of(compoundTerm))
            }
            calculator(
                compoundTerm.arguments[0].evaluateAsMathematicalExpression(context),
                compoundTerm.arguments[1].evaluateAsMathematicalExpression(context),
                context,
            )
        }
    }

    fun registerOperator(operatorName: String, calculator: (PrologNumber, PrologNumber) -> PrologNumber) {
        registerContextualOperator(operatorName) { a, b, _ -> calculator(a, b) }
    }

    init {
        // common binary operators
        registerContextualOperator("+", PrologNumber::plus)
        registerContextualOperator("-", PrologNumber::minus)
        registerContextualOperator("*", PrologNumber::times)
        registerContextualOperator("/", PrologNumber::div)
        registerContextualOperator("mod", PrologNumber::rem)
        registerContextualOperator("^", PrologNumber::toThe)

        registerOperator("+", PrologNumber::unaryPlus)
        registerOperator("-", PrologNumber::unaryMinus)

        registerOperator("ceil", PrologNumber::ceil)
        registerOperator("floor", PrologNumber::floor)
        registerContextualOperator("sqrt", PrologNumber::sqrt)
    }
}
