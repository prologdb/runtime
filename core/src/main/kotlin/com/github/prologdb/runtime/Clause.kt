package com.github.prologdb.runtime

import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.util.ArityMap
import java.util.WeakHashMap

interface Clause : PrologCallable

/**
 * A indicator of a clause, e.g. `likes/2`.
 */
data class ClauseIndicator internal constructor(
    val functor: String,
    val arity: Int
) {
    init {
        require(functor.isNotBlank())
        require(arity >= 0)
    }

    override fun toString() = "$functor/$arity"

    fun toIdiomatic(): CompoundTerm = CompoundTerm("/", arrayOf(Atom(functor), PrologInteger.createUsingStringOptimizerCache(arity.toLong())))

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

        @JvmStatic
        fun ofIdiomatic(term: Term, errorReference: String): ClauseIndicator {
            if (term !is CompoundTerm || term.arity != 2 || term.functor != "/") {
                throw PrologRuntimeException("Predicate indicators in $errorReference must be instances of `/`/2")
            }

            val functorTerm = term.arguments[0]
            val arityTerm = term.arguments[1]

            val functor = when (functorTerm) {
                is Atom -> functorTerm.name
                else -> throw PrologRuntimeException("Predicate functors in $errorReference must be atoms, got ${functorTerm.prologTypeName}")
            }

            val arityLong = when (arityTerm) {
                is PrologInteger -> arityTerm.value
                else -> throw PrologRuntimeException("Predicate arities in $errorReference must be integers, got ${functorTerm.prologTypeName}")
            }

            if (arityLong < 0) {
                throw PrologRuntimeException("Predicate arities in $errorReference cannot be negative")
            }

            if (arityLong > Int.MAX_VALUE) {
                throw PrologRuntimeException("Predicate arities in $errorReference must be less than or equal to ${Int.MAX_VALUE}")
            }

            return of(functor, arityLong.toInt())
        }
    }
}

/**
 * Like [ClauseIndicator] but adds the parent module name. Fully qualified clause indicators always refer to the
 * predicate name local to the declaring module (so that the name is unambiguous and canonical)
 */
data class FullyQualifiedClauseIndicator(
    val moduleName: String,
    val indicator: ClauseIndicator
) {
    init {
        require(moduleName.isNotBlank())
    }

    override fun toString() = "$moduleName/$indicator"

    fun toIdiomatic() = CompoundTerm("/", arrayOf(indicator.toIdiomatic()))
}
