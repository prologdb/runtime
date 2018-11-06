package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

interface ClauseStore {
    /**
     * All clauses defined in this store. Used mainly for `listing/0`.
     */
    val clauses: Iterable<Clause>

    /**
     * Finds entries within [clauses] that possibly unify with the given [Predicate] (facts or rule heads). This
     * method may just return [clauses] but may also implement sophisticated indexing or involve persistent storage.
     *
     * The default implementation of this method uses the kotlin stdlib [filter] method.
     */
    fun findFor(predicate: Predicate): Iterable<Clause> = clauses.filter { it.arity == predicate.arity && it.name == predicate.name }
}

interface MutableClauseStore : ClauseStore {
    /**
     * Adds the given entry to the library; acts like `assertz/1`: adds the entry to the **end** of the knowledge base.
     */
    fun assertz(entry: Clause)

    /**
     * Removes facts that unify with the given [Predicate] from the entry store.
     *
     * *Does not affect rules.*.
     *
     * @return Unifications of the given [Predicate] with the removed facts. Every invocation of [LazySequence.tryAdvance]
     *         attempts to remove another entry from the store.
     */
    fun retractFact(fact: Predicate): LazySequence<Unification>

    /**
     * Removes facts (which unify with the given [Predicate]) **or rules (whichs heads unify with
     * the given predicate)** from this store. Acts like `retract/1`.
     *
     * @return The unifications of the given [Predicate] with the removed facts resp. rule heads. Every invocation
     *         of [LazySequence.tryAdvance] attempts to remove another entry from the store.
     */
    fun retract(unifiesWith: Predicate): LazySequence<Unification>

    /**
     * Removes all facts that unify with the given [Predicate] from the entry store. **Does not affect rules**.
     */
    fun retractAllFacts(fact: Predicate) {
        retractFact(fact).consumeAll()
    }

    /**
     * Removes all facts (which unify with the given [Predicate]) **and rules (whichs heads unify with the given
     * [Predicate])** from this store. Acts like `retractAll/1`.
     */
    fun retractAll(unifiesWith: Predicate) {
        retract(unifiesWith).consumeAll()
    }

    /**
     * Removes all facts with the given functor and arity. **Does not affect rules.**
     */
    fun abolishFacts(functor: String, arity: Int) {
        retractAllFacts(Predicate(functor, Array<Term>(arity, { Variable.ANONYMOUS })))
    }

    /**
     * Removes all facts **and rules** with the given functor and arity. Acts like `abolish/1` and `abolish/2`.
     */
    fun abolish(functor: String, arity: Int) {
        retractAll(Predicate(functor, Array<Term>(arity, { Variable.ANONYMOUS })))
    }
}

private typealias ArityMapToLibraryEntries = ArityMap<MutableList<Clause>>

/**
 * The most simple implementation of [MutableClauseStore] possible: is
 * based on a plain [MutableList] and uses the default implementations
 * declared in [ReadableClauseStore] and [MutableClauseStore]
 */
class SimpleClauseStore(givenEntries: Iterable<Clause> = emptyList()) : MutableClauseStore {
    private val entries = ArrayList(givenEntries.toList())

    override val clauses = entries

    override fun assertz(entry: Clause) {
        entries.add(entry)
    }

    override fun retractFact(fact: Predicate): LazySequence<Unification> {
        return LazySequence.fromGenerator {
            for (index in 0 until entries.size) {
                val entry = entries[index]
                if (entry is Predicate) {
                    val unification = entry.unify(fact)
                    if (unification != null) {
                        entries.removeAt(index)
                        return@fromGenerator unification
                    }
                }
            }

            null
        }
    }

    override fun retract(unifiesWith: Predicate): LazySequence<Unification> {
        return LazySequence.fromGenerator {
            for (index in 0 until entries.size) {
                val entry = entries[index]
                if (entry is Predicate) {
                    val unification = entry.unify(unifiesWith)
                    if (unification != null) {
                        entries.removeAt(index)
                        return@fromGenerator unification
                    }
                } else if (entry is Rule) {
                    val headUnification = entry.head.unify(unifiesWith)
                    if (headUnification != null) {
                        entries.removeAt(index)
                        return@fromGenerator headUnification
                    }
                } else {
                    throw PrologRuntimeException("Cannot test whether to retract an entry: is neither a fact nor a rule")
                }
            }

            null
        }
    }

    override fun abolishFacts(functor: String, arity: Int) {
        entries.removeAll(entries.filter { it.arity == arity && it is Predicate && it.name == functor  })
    }

    override fun abolish(functor: String, arity: Int) {
        entries.removeAll(entries.filter { it.arity == arity && it.name == functor })
    }
}

/**
 * Indexes entries by arity and name in maps.
 */
class DoublyIndexedClauseStore : MutableClauseStore {
    /**
     * The index. The key in the outer map corresponds to [Clause.name] and the
     * key of the inner map corresponds to [Clause.arity].
     */
    private val index = mutableMapOf<String, ArityMapToLibraryEntries>()

    override fun findFor(predicate: Predicate): Iterable<Clause> {
        val arityMap = index[predicate.name] ?: return emptyList()
        return arityMap[predicate.arity] ?: emptyList()
    }

    override fun assertz(entry: Clause) {
        val arityIndex: ArityMapToLibraryEntries

        if (entry.name !in index) {
            arityIndex = ArityMap()
            index[entry.name] = arityIndex
        } else {
            arityIndex = index[entry.name]!!
        }

        val entryList: MutableList<Clause>

        if (entry.arity !in arityIndex) {
            entryList = mutableListOf()
            arityIndex[entry.arity] = entryList
        } else {
            entryList = arityIndex[entry.arity]!!
        }

        entryList.add(entry)
        indexChangedSinceLastExportsCalculation = true
    }

    private var indexChangedSinceLastExportsCalculation = true
    private var cachedExports: Iterable<Clause>? = null
    override val clauses: Iterable<Clause>
        get() {
            if (indexChangedSinceLastExportsCalculation) {
                cachedExports = index.flatMap { it.value.arities.flatMap { arity -> it.value[arity]!! }}
                indexChangedSinceLastExportsCalculation = false
            }

            return cachedExports!!
        }

    override fun retractFact(fact: Predicate): LazySequence<Unification> {
        val arityIndex = index[fact.name] ?: return Unification.NONE
        val entryList = arityIndex[fact.arity] ?: return Unification.NONE

        return LazySequence.fromGenerator {
            for (index in 0 until entryList.size) {
                val entry = entryList[index]
                if (entry is Predicate) {
                    val unification = entry.unify(fact)
                    if (unification != null) {
                        entryList.removeAt(index)
                        return@fromGenerator unification
                    }
                }
            }

            null
        }
    }

    override fun retract(unifiesWith: Predicate): LazySequence<Unification> {
        val arityIndex = index[unifiesWith.name] ?: return Unification.NONE
        val entryList = arityIndex[unifiesWith.arity] ?: return Unification.NONE

        return LazySequence.fromGenerator {
            for (index in 0 until entryList.size) {
                val entry = entryList[index]
                if (entry is Predicate) {
                    val unification = entry.unify(unifiesWith)
                    if (unification != null) {
                        entryList.removeAt(index)
                        return@fromGenerator unification
                    }
                } else if (entry is Rule) {
                    val headUnification = entry.head.unify(unifiesWith)
                    if (headUnification != null) {
                        entryList.removeAt(index)
                        return@fromGenerator headUnification
                    }
                } else {
                    throw PrologRuntimeException("Cannot determine whether entry should be retracted: is neither a predicate nor a rule.")
                }
            }

            null
        }
    }

    override fun abolish(functor: String, arity: Int) {
        index[functor]?.remove(arity)
    }

    override fun abolishFacts(functor: String, arity: Int) {
        val arityMap = index[functor] ?: return
        val entryList = arityMap[arity] ?: return

        entryList.removeAll(entryList.filter { it is Predicate })
    }
}