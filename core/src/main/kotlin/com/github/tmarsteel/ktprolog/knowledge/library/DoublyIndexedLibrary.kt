package com.github.tmarsteel.ktprolog.knowledge.library

import com.github.tmarsteel.ktprolog.term.Predicate
import kotlin.coroutines.experimental.buildSequence

/**
 * Indexes entries by arity and name in maps.
 */
class DoublyIndexedLibrary : MutableLibrary {

    /**
     * The index. The key in the outer map corresponds to [LibraryEntry.name] and the
     * key of the inner map corresponds to [LibraryEntry.arity].
     */
    private val index = mutableMapOf<String,ArityMap>()

    override fun findFor(predicate: Predicate): Iterable<LibraryEntry> {
        val arityMap = index[predicate.name] ?: return emptyList()
        return arityMap[predicate.arity] ?: emptyList()
    }

    override fun add(entry: LibraryEntry) {
        val arityIndex: ArityMap

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

    /**
     * A simplified [Map]<Integer,T>.
     *
     * The arity of prolog predicates is expected to be in the lower positive range. Because of that,
     * reserving O(n) memory for the index where n is the number of different arities in the map will
     * be efficient. As a result, a [Array]<T> is used for its performance advantages.
     */
    class ArityMap {

        private val itemsResizeMutex = Any()

        private var items = Array<MutableList<LibraryEntry>?>(6, {null})

        val arities: Iterable<Int> = buildSequence {
            (0 until items.size)
                .filter { items[it] != null }
                .forEach { yield(it) }
        }.asIterable()

        operator fun get(arity: Int): MutableList<LibraryEntry>? {
            if (arity < 0) {
                throw IllegalArgumentException("The arity must positive or 0.")
            }

            if (arity >= items.size) {
                return null
            }

            return items[arity]!!
        }

        operator fun set(arity: Int, item: MutableList<LibraryEntry>) {
            if (arity < 0) {
                throw IllegalArgumentException("The arity must positive or 0.")
            }

            if (arity >= items.size) {
                synchronized(itemsResizeMutex) {
                    items = Array(arity + 1, { index -> if (index < items.size) items[index] else null })
                }
            }

            items[arity] = item
        }

        operator fun contains(arity: Int): Boolean {
            if (arity < 0) {
                throw IllegalArgumentException("The arity must positive or 0.")
            }

            if (arity >= items.size) {
                return false
            }

            return items[arity] != null
        }

        /**
         * Creates a reference copy of this map. Changes to the copy do not
         * reflect onto this map; changes on the [LibraryEntry]s within **do**.
         */
        fun copy(): ArityMap {
            val copy = ArityMap()
            copy.items = Array(this.items.size, {
                val listCopy = mutableListOf<LibraryEntry>()
                listCopy.addAll(this.items[it]!!)
                listCopy
            })
            return copy
        }
    }
}