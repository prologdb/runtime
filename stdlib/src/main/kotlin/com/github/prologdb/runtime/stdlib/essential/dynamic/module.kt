package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.parser.lexer.Operator
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.builtin.prologSourceInformation
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term

/**
 * Converts compound terms (instances of `,/2` and `;/2` to
 * queries).
 */
internal fun compoundToQuery(compoundTerm: CompoundTerm): Query {
    val sourceInformation = compoundTerm.sourceInformation.orElse { getInvocationStackFrame().prologSourceInformation }

    if (compoundTerm.arity != 2) {
        return PredicateInvocationQuery(compoundTerm, sourceInformation)
    }

    if (compoundTerm.functor == Operator.COMMA.text || compoundTerm.functor == Operator.SEMICOLON.text) {
        val allArgumentsCompound = compoundTerm.arguments.all { it is CompoundTerm }
        if (!allArgumentsCompound) {
            return PredicateInvocationQuery(compoundTerm, sourceInformation)
        }

        val argumentsConverted = compoundTerm.arguments.map { compoundToQuery(it as CompoundTerm) }.toTypedArray()
        return when (compoundTerm.functor) {
            Operator.COMMA.text -> AndQuery(argumentsConverted)
            Operator.SEMICOLON.text -> OrQuery(argumentsConverted)
            else -> throw IllegalStateException()
        }
    }
    // else:
    return PredicateInvocationQuery(compoundTerm, sourceInformation)
}

internal fun Term.commaCompoundToList(): List<Term> {
    val list = mutableListOf<Term>()
    var pivot = this
    while (pivot is CompoundTerm && pivot.arity == 2 && pivot.functor == Operator.COMMA.text) {
        list.add(pivot.arguments[0])
        pivot = pivot.arguments[1]
    }

    list.add(pivot)

    return list
}
