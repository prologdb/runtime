package com.github.tmarsteel.ktprolog.knowledge.library

import com.github.tmarsteel.ktprolog.ArityMap
import com.github.tmarsteel.ktprolog.term.Predicate

/**
 * Indexes entries by arity and name in maps.
 */
class DoublyIndexedLibrary : MutableLibrary {

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

    override fun include(otherLibrary: Library) {
        if (otherLibrary is DoublyIndexedLibrary) {
            // merge the indexes
            for ((name, othersArityMap) in otherLibrary.index) {
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
            otherLibrary.exports.forEach(this::add)
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
