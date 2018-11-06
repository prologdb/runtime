package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.ArityMap
import java.util.*

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

        fun of(clause: HasNameAndArity): ClauseIndicator = of (clause.name, clause.arity)
    }
}