package com.github.prologdb.runtime.util

import java.util.concurrent.atomic.AtomicReferenceArray
import kotlin.coroutines.experimental.buildSequence

/**
 * A simplified [Map]<Integer,T>, thread safe.
 *
 * The arity of compound terms is expected to be in the 1-digit positive range (2-digit at most) Because of that,
 * reserving O(n) memory for the index where n is the number of different arities in the map will
 * be efficient. As a result, a [Array]<T> is used for its performance advantages.
 */
open class ArityMap<T>(givenItems: Collection<T> = emptySet()) {

    private var items: AtomicReferenceArray<T?> = run {
        val initialCapacity = if (givenItems.isEmpty()) 6 else givenItems.size
        AtomicReferenceArray(initialCapacity)
    }

    private val itemsResizeMutex = Any()

    /**
     * The maximum arity this map can store without allocating additional memory.
     * The capacity cannot be reduced; the setter will not throw but also have no effect.
     */
    var capacity: Int
        get() = items.length() - 1
        set(value) {
            if (value <= items.length() - 1) return

            synchronized(itemsResizeMutex) {
                val oldSize = items.length()
                val newItems = Array<Any?>(value + 1, { index -> if (index < oldSize) items[index] else null }) as Array<T?>
                items = AtomicReferenceArray(newItems)
            }
        }

    val arities: Iterable<Int> = buildSequence {
        (0 until items.length())
            .filter { items[it] != null }
            .forEach { yield(it) }
    }.asIterable()

    operator fun get(arity: Int): T? {
        if (arity < 0) {
            throw IllegalArgumentException("The arity must positive or 0.")
        }

        if (arity >= items.length()) {
            return null
        }

        return items[arity]
    }

    operator fun set(arity: Int, item: T) {
        if (arity < 0) {
            throw IllegalArgumentException("The arity must positive or 0.")
        }

        if (arity >= items.length()) {
            capacity = arity
        }

        items[arity] = item
    }

    operator fun contains(arity: Int): Boolean {
        if (arity < 0) {
            throw IllegalArgumentException("The arity must positive or 0.")
        }

        if (arity >= items.length()) {
            return false
        }

        return items[arity] != null
    }

    fun remove(arity: Int) {
        if (arity < 0) {
            throw IllegalArgumentException("The arity must be positive or 0")
        }

        if (arity < items.length()) {
            items[arity] = null
        }
    }

    fun values(): Iterable<T> = (0 until items.length())
        .map(items::get)
        .filterNot { it == null } as Iterable<T>
}
