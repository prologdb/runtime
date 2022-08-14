package com.github.prologdb.runtime.stdlib

import com.github.prologdb.parser.lexer.Operator
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.builtin.prologSourceInformation
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

class TypedPredicateArguments(val indicator: ClauseIndicator, val raw: Array<out Term>) {
    val size = raw.size
    operator fun get(index: Int): Term = raw[index]

    fun <T : Term> getTyped(index: Int, type: Class<T>): T {
        val untyped = this[index]
        if (type.isInstance(untyped)) {
            @Suppress("unchecked_cast")
            return untyped as T
        }

        throw ArgumentTypeError(indicator, index, untyped, type)
    }

    inline fun <reified T : Term> getTyped(index: Int) = getTyped(index, T::class.java)

    fun <T : Term> getTypedOrUnbound(index: Int, type: Class<T>): Term {
        val term = this[index]
        if (term is Variable) {
            return term
        }
        if (type.isInstance(term)) {
            return term
        }

        throw ArgumentTypeError(indicator, index, term, type, Variable::class.java)
    }

    inline fun <reified T: Term> getTypedOrUnbound(index: Int): Term = getTypedOrUnbound(index, T::class.java)

    fun getQuery(index: Int): Query {
        return compoundToQuery(getTyped(index), index)
    }

    /**
     * Parses a goal of the form `Var1^Var2^Goal`, where there can be an arbitrary number
     * of variables (including none) prepended by instances of `^/2`. If the first argument to a `^/2` instance
     * is not a [Variable], that term is ignored.
     */
    fun getQueryWithExistentialVariables(index: Int): Pair<Query, Set<Variable>> {
        val variables = mutableSetOf<Variable>()
        var pivot = get(index)
        while (pivot is CompoundTerm && pivot.functor == "^" && pivot.arity == 2) {
            val variable = pivot.arguments[0]
            if (variable is Variable) {
                variables.add(variable)
            }

            pivot = pivot.arguments[1]
        }

        if (pivot !is CompoundTerm) {
            throw ArgumentTypeError(index, pivot, CompoundTerm::class.java)
        }

        return Pair(compoundToQuery(pivot, index), variables)
    }

    private fun compoundToQuery(compoundTerm: CompoundTerm, argumentIndex: Int): Query {
        val sourceInformation = compoundTerm.sourceInformation.orElse { getInvocationStackFrame().prologSourceInformation }

        if (compoundTerm.arity != 2) {
            return PredicateInvocationQuery(compoundTerm, sourceInformation)
        }

        if (compoundTerm.functor == Operator.COMMA.text || compoundTerm.functor == Operator.SEMICOLON.text) {
            val allArgumentsCompound = compoundTerm.arguments.all { it is CompoundTerm }
            if (!allArgumentsCompound) {
                return PredicateInvocationQuery(compoundTerm, sourceInformation)
            }

            val argumentsConverted = compoundTerm.arguments.map { compoundToQuery(it as CompoundTerm, argumentIndex) }.toTypedArray()
            return when (compoundTerm.functor) {
                Operator.COMMA.text -> AndQuery(argumentsConverted)
                Operator.SEMICOLON.text -> OrQuery(argumentsConverted)
                else -> throw ArgumentError(argumentIndex, "expected comma or semicolon, got compound term ${compoundTerm.functor}")
            }
        }
        // else:
        return PredicateInvocationQuery(compoundTerm, sourceInformation)
    }
}
