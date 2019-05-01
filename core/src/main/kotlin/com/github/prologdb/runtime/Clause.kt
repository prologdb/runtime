package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.util.ArityMap
import java.util.*

interface Clause : HasFunctorAndArity {
    /**
     * Unifies the given compound (`other`) with this entry; if this is a fact (a [CompoundTerm]), unifies with
     * the given compound and ignores the given [ProofSearchContext]. If this is a rule, uses the [ProofSearchContext]
     * to run the query (in case the head and the given [CompoundTerm] unify).
     */
    val unifyWithKnowledge: suspend LazySequenceBuilder<Unification>.(other: CompoundTerm, context: ProofSearchContext) -> Unit
}

/**
 * A indicator of a clause, e.g. `likes/2`.
 */
data class ClauseIndicator internal constructor(
    val functor: String,
    val arity: Int
) {
    override fun toString() = "$functor/$arity"

    fun toIdiomatic(): CompoundTerm = object : CompoundTerm("/", arrayOf(Atom(functor), PrologInteger(arity.toLong()))) {
        override fun toString() = this@ClauseIndicator.toString()
    }

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

            if (parts[0].isBlank()) throw IllegalArgumentException("\"$indicatorStr\" is not a valid clause indicator: functor is blank")
            val arity = parts[1].toIntOrNull() ?: throw IllegalArgumentException("\"$indicatorStr\" is not a valid caluse indicator: arity is not numeric.")
            return of(parts[0], arity)
        }

        /**
         * @return An instance of [ClauseIndicator] where [name] and [arity]
         * equal the given arguments. Utilizes a weak-reference cache.
         */
        fun of(name: String, arity: Int): ClauseIndicator {
            // assure functor-map is present
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

        fun of(clause: HasFunctorAndArity): ClauseIndicator = of(clause.functor, clause.arity)
    }
}