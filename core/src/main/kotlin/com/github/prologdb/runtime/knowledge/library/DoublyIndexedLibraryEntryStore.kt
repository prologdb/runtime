package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.ArityMap
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

/**
 * Indexes entries by arity and name in maps.
 */
class DoublyIndexedLibraryEntryStore : MutableLibraryEntryStore {
    /**
     * The index. The key in the outer map corresponds to [LibraryEntry.name] and the
     * key of the inner map corresponds to [LibraryEntry.arity].
     */
    private val index = mutableMapOf<String, ArityMapToLibraryEntries>()

    override fun findFor(predicate: Predicate): Iterable<LibraryEntry> {
        val arityMap = index[predicate.name] ?: return emptyList()
        return arityMap[predicate.arity] ?: emptyList()
    }

    override fun add(entry: LibraryEntry) {
        val arityIndex: ArityMapToLibraryEntries

        if (entry.name !in index) {
            arityIndex = ArityMap()
            index[entry.name] = arityIndex
        } else {
            arityIndex = index[entry.name]!!
        }

        val entryList: MutableList<LibraryEntry>

        if (entry.arity !in arityIndex) {
            entryList = mutableListOf()
            arityIndex[entry.arity] = entryList
        } else {
            entryList = arityIndex[entry.arity]!!
        }

        entryList.add(entry)
        indexChangedSinceLastExportsCalculation = true
    }

    override fun include(other: LibraryEntryStore) {
        if (other is DoublyIndexedLibraryEntryStore) {
            // merge the indexes
            for ((name, othersArityMap) in other.index) {
                if (name in this.index) {
                    val thisArityMap = this.index[name]!!
                    for (arity in othersArityMap.arities) {
                        if (arity !in thisArityMap) {
                            thisArityMap[arity] = mutableListOf()
                        }

                        thisArityMap[arity]!!.addAll(othersArityMap[arity]!!)
                    }
                }
                else
                {
                    this.index[name] = othersArityMap.copy()
                }
            }
        } else {
            other.exports.forEach(this::add)
        }

        indexChangedSinceLastExportsCalculation = true
    }

    private var indexChangedSinceLastExportsCalculation = true
    private var cachedExports: Iterable<LibraryEntry>? = null
    override val exports: Iterable<LibraryEntry>
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
                        return@fromGenerator  headUnification
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

        entryList.removeAll(entryList.filter { it is Predicate})
    }
}

private typealias ArityMapToLibraryEntries = ArityMap<MutableList<LibraryEntry>>

/**
 * Creates a reference copy of this map. Changes to the copy do not
 * reflect onto this map; changes on the [LibraryEntry]s within **do**.
 */
private fun ArityMapToLibraryEntries.copy() =
    ArityMap<MutableList<LibraryEntry>>(kotlin.Array(capacity + 1, {
        val listCopy = kotlin.collections.mutableListOf<LibraryEntry>()
        listCopy.addAll(this[it]!!)
        listCopy
    }))