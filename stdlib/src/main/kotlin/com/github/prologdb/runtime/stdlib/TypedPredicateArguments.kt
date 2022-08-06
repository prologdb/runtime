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

class TypedPredicateArguments(val indicator: ClauseIndicator, val raw: Array<out Term>) {
    val size = raw.size
    operator fun get(index: Int): Term = raw[index]

    fun <T : Term> getTyped(index: Int, type: Class<T>): T {
        val untyped = this[index]
        if (type.isInstance(untyped)) {
            @Suppress("unchecked_cast")
            return untyped as T
        } else {
            throw ArgumentTypeError(indicator, index, untyped, type)
        }
    }

    inline fun <reified T : Term> getTyped(index: Int) = getTyped(index, T::class.java)

    fun getQuery(index: Int): Query {
        return compoundToQuery(getTyped(index), index)
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
