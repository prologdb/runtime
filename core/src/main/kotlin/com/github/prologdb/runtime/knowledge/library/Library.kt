package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.term.CompoundTerm

/**
 * A library of predicates. Intended to be used for static code (such as read from a .pl file),
 * has no facilities for dynamic updates.
 *
 * For dynamic predicates use [ClauseStore].
 */
class Library(
    /** A name for the library, e.g. `lists` */
    val name: String,

    /**
     * The clauses that make up this library. The order exposed by this
     * iterable also defines the order in which the clauses will be considered
     * for proof-search.
     */
    givenClauses: Iterable<Clause>,

    /**
     * The types of clauses (not necessarily a subset of [exports]) that this
     * library intends to be dynamic (can be modified by user code,
     * affecting only the user knowledge base, not this library instance).
     */
    val dynamicExports: Set<ClauseIndicator>,

    /**
     * The operators defined in this registry (using `op/3`). Will be registered
     * with the knowledge base upon loading.
     */
    val operators: OperatorRegistry
) {
    /**
     * The types of clauses exported by this library.
     */
    val exports: Set<ClauseIndicator> = givenClauses.asSequence()
        .map { ClauseIndicator.of(it) }
        .toSet()

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
     * Finds clauses in this library that are likely to unify with the given [CompoundTerm] (facts or rule heads).
     *
     * "likely" means at least same name and arity; further optimizations are
     * implementation detail.
     */
    fun findFor(compoundTerm: CompoundTerm): Iterable<Clause> {
        return index[compoundTerm.arity]?.get(compoundTerm.name) ?: emptyList()
    }

    /**
     * Finds all clauses for the given indicator.
     *
     * The behaviour of this method is undefined for indicators that
     * are not listed in [exports].
     */
    fun findFor(indicator: ClauseIndicator): Iterable<Clause> {
        return index[indicator.arity]?.get(indicator.name) ?: emptyList()
    }
}