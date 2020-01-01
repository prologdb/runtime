package com.github.prologdb.runtime.analyzation

import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.util.crossover

/**
 * @return whether any invocation to a predicate consisting of the given clauses
 * will invoke none or **exactly** one of the clauses (not more!).
 */
fun Collection<Clause>.areMutuallyExclusive(): Boolean {
    if (size <= 1) return true

    val randomVarsScope = RandomVariableScope()

    fun Clause.head(): CompoundTerm = when (this) {
        is CompoundTerm -> this
        is Rule         -> this.head
        else            -> throw IllegalArgumentException()
    }

    return this
        .crossover { clause1, clause2 ->
            val clause1randomHead = randomVarsScope.withRandomVariables(clause1.head(), VariableMapping())
            return@crossover clause1randomHead.unify(clause2.head(), randomVarsScope) != null
        }
        .none { it }
}