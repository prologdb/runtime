package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.term.Number
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

/**
 * Takes an arithmetic predicate (e.g. +(1,2) or mod(23,4)) and returns the calculated value
 */
typealias Calculator = (Predicate) -> Number

val MathLibrary : Library = object : SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry()) {

    init {
        add(IsRule)
        add(LessThanPredicate)
        add(LessThanOrEqualPredicate)
        add(GreaterThanPredicate)
        add(GreaterThanOrEqualPredicate)

        defineOperator(OperatorDefinition(700, OperatorType.XFX, "<"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "=<"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "=\\="))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, ">"))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, ">="))
        defineOperator(OperatorDefinition(700, OperatorType.XFX, "is"))

        defineOperator(OperatorDefinition(500, OperatorType.YFX, "+"))
        defineOperator(OperatorDefinition(500, OperatorType.YFX, "-"))
        defineOperator(OperatorDefinition(500, OperatorType.YFX, "xor"))

        defineOperator(OperatorDefinition(400, OperatorType.YFX, "*"))

        defineOperator(OperatorDefinition(400, OperatorType.YFX, "mod"))

        defineOperator(OperatorDefinition(200, OperatorType.XFX, "**"))

        defineOperator(OperatorDefinition(200, OperatorType.XFY, "^"))

        defineOperator(OperatorDefinition(200, OperatorType.FY, "+"))
        defineOperator(OperatorDefinition(200, OperatorType.FY, "-"))
    }
}

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
        registerOperator("+",   Number::plus)
        registerOperator("-",   Number::minus)
        registerOperator("*",   Number::times)
        registerOperator("/",   Number::div)
        registerOperator("mod", Number::rem)
        registerOperator("^",   Number::toThe)

        registerOperator("+", Number::unaryPlus)
        registerOperator("-", Number::unaryMinus)
    }
}

private object IsPredicate : BuiltinPredicate("is", A, B)

object IsRule : Rule(IsPredicate, PredicateQuery(IsPredicate)) {
    override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        val randomMapping = VariableMapping()
        val randomPredicate = randomVariableScope.withRandomVariables(predicate, randomMapping)
        val predicateAndHeadUnification = head.unify(randomPredicate) ?: return Unification.NONE

        val valForA = predicateAndHeadUnification.variableValues[A]
        val valForB = predicateAndHeadUnification.variableValues[B]
        val bucket = VariableBucket()

        if (valForA is Variable) {
            bucket.instantiate(valForA, valForB.asNumber)
            return LazySequence.of(Unification(bucket.withVariablesResolvedFrom(randomMapping)))
        }
        else if (valForB is Variable) {
            bucket.instantiate(valForB, valForA.asNumber)
            return LazySequence.of(Unification(bucket.withVariablesResolvedFrom(randomMapping)))
        }
        else if (valForA is Number) {
            if (valForB.asNumber == valForA) {
                return Unification.SINGLETON
            } else {
                return Unification.NONE
            }
        }
        else if (valForB is Number) {
            if (valForA.asNumber == valForB) {
                return Unification.SINGLETON
            } else {
                return Unification.NONE
            }
        }
        else {
            return Unification.NONE
        }
    }
}

val Term.asNumber: Number
    get() = when(this) {
        is Atom -> throw PrologRuntimeException("is/2: $this is not a number")
        is Number -> this
        is com.github.prologdb.runtime.term.List -> throw PrologRuntimeException("is/2: $this is not a number")
        is Variable -> throw PrologRuntimeException("is/2: Arguments not sufficiently instantiated: $this")
        is Predicate -> MathOperatorRegistry.evaluate(this)
        else -> throw PrologRuntimeException("is/2: cannot evaluate term $this")
    }

object GreaterThanPredicate : BuiltinPredicate(">", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}

private object GreaterThanOrEqualPredicate : BuiltinPredicate(">=", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}

private object LessThanPredicate : BuiltinPredicate("<", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}

object LessThanOrEqualPredicate : BuiltinPredicate("=<", A, B) {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        val unification = super.unify(rhs, randomVarsScope) ?: return Unification.FALSE
        val valForA = unification.variableValues[A]
        val valForB = unification.variableValues[B]

        if (valForA.asNumber > valForB.asNumber) {
            return Unification.TRUE
        } else {
            return Unification.FALSE
        }
    }
}