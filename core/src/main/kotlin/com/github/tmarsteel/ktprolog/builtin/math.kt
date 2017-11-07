package com.github.tmarsteel.ktprolog.builtin

import com.github.tmarsteel.ktprolog.ArityMap
import com.github.tmarsteel.ktprolog.PrologRuntimeException
import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.knowledge.KnowledgeBase
import com.github.tmarsteel.ktprolog.knowledge.Rule
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.query.PredicateQuery
import com.github.tmarsteel.ktprolog.term.*
import com.github.tmarsteel.ktprolog.term.Number
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.unification.VariableBucket

/**
 * Takes an arithmetic predicate (e.g. +(1,2) or mod(23,4)) and returns the calculated value
 */
typealias Calculator = (Predicate) -> Number

object MathLibrary : Library {
    override val exports = listOf(
        IsRule
    )

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
        val calculator = getCalculator(predicate.name, predicate.arity) ?: throw PrologRuntimeException("Arithmetic operator ${predicate.name}/{$predicate.arity} is not defined")
        return calculator(predicate)
    }

    fun registerOperator(operatorName: String, calculator: (Number) -> Number) {
        registerOperator(operatorName, 1..1, { predicate ->
            if (predicate.arity != 1 || predicate.name != operatorName) throw PrologRuntimeException("Calculator for $operatorName/1 cannot be invoked with an instance of ${predicate.name}/${predicate.arguments.size}")
            calculator(predicate.arguments[0].asNumber)
        })
    }

    fun registerOperator(operatorName: String, calculator: (Number, Number) -> Number) {
        registerOperator(operatorName, 2..2, { predicate ->
            if (predicate.arity != 2 || predicate.name != operatorName) throw PrologRuntimeException("Calculator for $operatorName/2 cannot be invoked with an instance of ${predicate.name}/${predicate.arguments.size}")
            calculator(predicate.arguments[0].asNumber, predicate.arguments[1].asNumber)
        })
    }

    init {
        // common binary operators
        registerOperator("+",   Number::plus)
        registerOperator("-",   Number::minus)
        registerOperator("*",   Number::times)
        registerOperator("/",   Number::div)
        registerOperator("mod", Number::rem)
        registerOperator("^",   Number::toThePowerOf)
    }
}

private object IsPredicate : BuiltinPredicate("is", A, B)

object IsRule : Rule(IsPredicate, PredicateQuery(IsPredicate)) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): Sequence<Unification> {
        val predicateAndHeadUnification = head.unify(predicate) ?: return Unification.NONE

        val valForA = predicateAndHeadUnification.variableValues[A]
        val valForB = predicateAndHeadUnification.variableValues[B]
        val bucket = VariableBucket()

        if (valForA is Variable) {
            bucket.instantiate(valForA, valForB.asNumber)
            return sequenceOf(Unification(bucket))
        }
        else if (valForB is Variable) {
            bucket.instantiate(valForB, valForA.asNumber)
            return sequenceOf(Unification(bucket))
        }
        else if (valForA is Number) {
            if (valForB.asNumber == valForA) {
                return sequenceOf(Unification.TRUE)
            } else {
                return emptySequence()
            }
        }
        else if (valForB is Number) {
            if (valForA.asNumber == valForB) {
                return sequenceOf(Unification.TRUE)
            } else {
                return emptySequence()
            }
        }
        else {
            return emptySequence()
        }
    }
}

val Term.asNumber: Number
    get() = when(this) {
        is Atom   -> throw PrologRuntimeException("is/2: $this is not a number")
        is Number -> this
        is com.github.tmarsteel.ktprolog.term.List -> throw PrologRuntimeException("is/2: $this is not a number")
        is Variable -> throw PrologRuntimeException("is/2: Arguments not sufficiently instantiated: $this")
        is Predicate -> MathLibrary.evaluate(this)
        else -> throw PrologRuntimeException("is/2: cannot evaluate term $this")
    }