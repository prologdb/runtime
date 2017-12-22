package com.github.prologdb.runtime

import kotlin.coroutines.experimental.buildSequence

/**
 * A simplified [Map]<Integer,T>.
 *
 * The arity of prolog predicates is expected to be in the lower positive range. Because of that,
 * reserving O(n) memory for the index where n is the number of different arities in the map will
 * be efficient. As a result, a [Array]<T> is used for its performance advantages.
 */
open class ArityMap<T>(private var items: Array<T?> = Array<Any?>(6, {null}) as Array<T?>) {

    private val itemsResizeMutex = Any()

    /**
     * The maximum arity this map can store without allocating additional memory
     */
    var capacity: Int
        get() = items.size - 1
        set(value) {
            synchronized(itemsResizeMutex) {
                items = Array<Any?>(value + 1, { index -> if (index < items.size) items[index] else null }) as Array<T?>
            }
        }

    val arities: Iterable<Int> = buildSequence {
        (0 until items.size)
            .filter { items[it] != null }
            .forEach { yield(it) }
    }.asIterable()

    operator fun get(arity: Int): T? {
        if (arity < 0) {
            throw IllegalArgumentException("The arity must positive or 0.")
        }

        if (arity >= items.size) {
            return null
        }

        return items[arity]
    }

    operator fun set(arity: Int, item: T) {
        if (arity < 0) {
            throw IllegalArgumentException("The arity must positive or 0.")
        }

        if (arity >= items.size) {
            capacity = arity
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

    fun remove(arity: Int) {
        if (arity < 0) {
            throw IllegalArgumentException("The arity must be positive or 0")
        }

        if (arity < items.size) {
            items[arity] = null
        }
    }

    fun values(): Iterable<T> = items.filter { it != null } as Iterable<T>
}