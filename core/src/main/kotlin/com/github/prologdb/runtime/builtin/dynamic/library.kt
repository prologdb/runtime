package com.github.prologdb.runtime.builtin.dynamic

import com.github.prologdb.runtime.builtin.nativeLibrary
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm

val DynamicsLibrary = nativeLibrary("dynamics") {
    add(BuiltinFindAll)
    add(BuiltinFindAllOptimized)
}

/**
 * Converts compund predicates (instances of `,/2` and `;/2` to
 * queries).
 */
fun predicateToQuery(compoundTerm: CompoundTerm): Query {
    if (compoundTerm.arity != 2) {
        return PredicateQuery(compoundTerm)
    }

    if (compoundTerm.name == "," || compoundTerm.name == ";") {
        val allArgumentsPredicates = compoundTerm.arguments.all { it is CompoundTerm }
        if (!allArgumentsPredicates) {
            return PredicateQuery(compoundTerm)
        }

        val argumentsConverted = compoundTerm.arguments.map { predicateToQuery(it as CompoundTerm) }.toTypedArray()
        return when (compoundTerm.name) {
            "," -> AndQuery(argumentsConverted)
            ";" -> OrQuery(argumentsConverted)
            else -> throw IllegalStateException()
        }
    }
    // else:
    return PredicateQuery(compoundTerm)
}