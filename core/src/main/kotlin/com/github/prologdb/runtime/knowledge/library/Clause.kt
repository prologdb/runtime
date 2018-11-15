package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import java.util.*

interface Clause : HasNameAndArity {
    /**
     * Unifies the given predicate (`other`) with this entry; if this is a fact (a [Predicate]), unifies with
     * the given predicate and ignores the given [KnowledgeBase]. If this is a rule, uses the [KnowledgeBase]
     * to run the query (in case the head and the given [Predicate] unify).
     */
    val unifyWithKnowledge: suspend LazySequenceBuilder<Unification>.(other: Predicate, context: ProofSearchContext) -> Unit
}

/**
 * A indicator of a clause, e.g. `likes/2`.
 */
data class ClauseIndicator private constructor(
    val name: String,
    val arity: Int
) {
    override fun toString() = "$name/$arity"

    companion object {
        /**
         * Cache for [of]
         */
        private val cache = ArityMap<MutableMap<String, ClauseIndicator>>()

        fun parse(indicatorStr: String): ClauseIndicator {
            val parts = indicatorStr.split('/')
            if (parts.size != 2) {
                throw IllegalArgumentException("\"$indicatorStr\" is not a valid clause indicator")
            }

            if (parts[0].isBlank()) throw IllegalArgumentException("\"$indicatorStr\" is not a valid clause indicator: name is blank")
            val arity = parts[1].toIntOrNull() ?: throw IllegalArgumentException("\"$indicatorStr\" is not a valid caluse indicator: arity is not numeric.")
            return of(parts[0], arity)
        }

        /**
         * @return An instance of [ClauseIndicator] where [name] and [arity]
         * equal the given arguments. Utilizes a weak-reference cache.
         */
        fun of(name: String, arity: Int): ClauseIndicator {
            // assure name-map is present
            if (arity !in cache) {
                synchronized(cache) {
                    if (arity !in cache) {
                        cache[arity] = WeakHashMap<String, ClauseIndicator>()
                    }
                }
            }

            val mapByName = cache[arity] ?: throw RuntimeException("Internal error; see stacktrace and source code.")
            return mapByName[name] ?: {
                val newly = ClauseIndicator(name, arity)
                mapByName[name] = newly
                newly
            }()
        }

        fun of(clause: HasNameAndArity): ClauseIndicator = of(clause.name, clause.arity)
    }
}