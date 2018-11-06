package com.github.prologdb.runtime.builtin.dynamic

import com.github.prologdb.runtime.knowledge.library.DefaultOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedClauseStore
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate

val DynamicsLibrary : Library = object : SimpleLibrary(DoublyIndexedClauseStore(), DefaultOperatorRegistry()) {
    init {
        add(BuiltinFindAll)
        add(BuiltinFindAllOptimized)
    }
}

/**
 * Converts compund predicates (instances of `,/2` and `;/2` to
 * queries).
 */
internal fun predicateToQuery(predicate: Predicate): Query {
    if (predicate.arity != 2) {
        return PredicateQuery(predicate)
    }

    if (predicate.name == "," || predicate.name == ";") {
        val allArgumentsPredicates = predicate.arguments.all { it is Predicate }
        if (!allArgumentsPredicates) {
            return PredicateQuery(predicate)
        }

        val argumentsConverted = predicate.arguments.map { predicateToQuery(it as Predicate) }.toTypedArray()
        return when (predicate.name) {
            "," -> AndQuery(argumentsConverted)
            ";" -> OrQuery(argumentsConverted)
            else -> throw IllegalStateException()
        }
    }
    // else:
    return PredicateQuery(predicate)
}