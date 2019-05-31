package com.github.prologdb.runtime.optimization

import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.PrologPredicate
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.util.crossover

class TailcallOptimizer {

    /**
     * Attempts to optimize a tailcall in the given predicate.
     *
     * @return A [PrologPredicate] identical behaviour as the given predicate and
     *         with tail call optimization applied. Returns `null` if the given predicate
     *         cannot be optimized.
     */
    fun tryOptimize(predicate: ASTPrologPredicate, forRuntime: PrologRuntimeEnvironment): PrologPredicate? {
        TODO()
    }

    /**
     * @return whether any invocation to a predicate consisting of the given clauses
     * will invoke none or **exactly** one of the clauses (not more!).
     */
    fun areMutuallyExclusive(clauses: Collection<Clause>): Boolean {
        if (clauses.size <= 1) return true

        val randomVarsScope = RandomVariableScope()

        return clauses
            .crossover { clause1, clause2 ->
                val clause1randomHead = randomVarsScope.withRandomVariables(clause1.head, VariableMapping())

                return@crossover clause1randomHead.unify(clause2.head, randomVarsScope) != null
            }
            .none { it }
    }
}

private val Clause.head: CompoundTerm
    get() = when (this) {
        is CompoundTerm -> this
        is Rule -> this.head
        else -> throw IllegalArgumentException()
    }
