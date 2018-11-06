package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.term.Predicate

class Library(
    /**
     * The clauses that make up this library. The order exposed by this
     * iterable also defines the order in which the clauses will be considered
     * for proof-search.
     */
    givenClauses: Iterable<Clause>,

    /**
     * The types of clauses (must be a subset of [exports]) that this
     * library intends to be protected (not amended by any other
     * clause of the same type from another source).
     */
    val protectedExports: Set<ClauseIndicator>
) {
    /**
     * The types of clauses exported by this library.
     */
    val exports: Set<ClauseIndicator> = givenClauses.asSequence()
        .map { ClauseIndicator.of(it) }
        .toSet()

    init {
        protectedExports.firstOrNull { it !in exports } ?.let {
            throw IllegalArgumentException("Cannot declare $it as protected without also defining clauses for it.")
        }
    }

    private val index: ArityMap<HashMap<String, ArrayList<Clause>>> = ArityMap()

    init {
        // fill the index
        for (clause in givenClauses) {
            val nameMap = index[clause.arity] ?: {
                val map = HashMap<String, ArrayList<Clause>>()
                index[clause.arity] = map
                map
            }()

            val list = nameMap[clause.name] ?: {
                val list = ArrayList<Clause>(2)
                nameMap[clause.name] = list
                list
            }()

            list.add(clause)
        }
    }

    /**
     * Finds clauses in this library that are likely to unify with the given [Predicate] (facts or rule heads).
     *
     * "likely" means at least same name and arity; further optimizations are
     * implementation detail.
     */
    fun findLikelyToUnifyWith(predicate: Predicate): Iterable<Clause> {
        return index[predicate.arity]?.get(predicate.name) ?: emptyList()
    }
}