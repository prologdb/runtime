package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.UnsupportedArgumentException

/**
 * Groups [T]s by a [Unification]. Optimizes with [Any.hashCode]  as long as the [Unification]s
 * are fully ground (see [Term.isGround]). When a non-ground [Unification] is introduced, all
 * operations slow down to `O(n)` performance.
 *
 * TODO: thread-safety
 */
interface UnificationGrouping<T : Any> : MutableIterable<Map.Entry<Unification, T>> {
    operator fun get(key: Unification): T?

    /**
     * @return the value previously associated with [key], or `null` if none.
     * @see MutableMap.put
     */
    operator fun set(key: Unification, value: T): T?
    val size: Int

    operator fun contains(key: Unification): Boolean = get(key) != null

    fun clear()

    /**
     * @return the value that was associated with the given [key], `null` if they [key] was not part of the grouping
     */
    fun remove(key: Unification): T?

    companion object {
        operator fun <T : Any> invoke(randomVariableScope: RandomVariableScope): UnificationGrouping<T> {
            return StrategyUnificationGrouping(HashMapUnificationGroupingStrategy(randomVariableScope))
        }
    }
}

private class StrategyUnificationGrouping<T : Any>(var strategy: UnificationGroupingStrategy<T>) : UnificationGrouping<T> {

    override fun get(key: Unification): T? = strategy.get(key)

    override fun set(key: Unification, value: T): T? {
        return try {
            strategy.set(key, value)
        } catch (ex: UnsupportedArgumentException) {
            strategy = strategy.upgradeFor(key)
            strategy.set(key, value)
        }
    }

    override fun iterator(): MutableIterator<Map.Entry<Unification, T>> = strategy.iterator()

    override val size: Int get() = strategy.size

    override fun clear() {
        strategy.clear()
    }

    override fun remove(key: Unification) = strategy.remove(key)
}

private interface UnificationGroupingStrategy<T : Any> : MutableIterable<Map.Entry<Unification, T>> {
    fun get(key: Unification): T?

    /**
     * Attempts to set the given value.
     * @return the value previously associated with [key], or `null` if [key] wasn't present before.
     * @see MutableMap.put
     * @throws UnsupportedArgumentException if this strategy doesn't support [key]. In that case, invoke [upgradeFor].
     */
    fun set(key: Unification, value: T): T?

    fun upgradeFor(key: Unification): UnificationGroupingStrategy<T>

    fun clear()

    fun remove(key: Unification): T?

    val size: Int
}

private class HashMapUnificationGroupingStrategy<T : Any>(
    private val randomVariableScope: RandomVariableScope,
) : UnificationGroupingStrategy<T> {
    private val groups = HashMap<Unification, T>()

    override val size: Int get() = groups.size

    override fun get(key: Unification): T? = groups[key]

    override fun set(key: Unification, value: T): T? {
        val compactedKey = key.compacted()
        if (!compactedKey.isGround) {
            throw UnsupportedArgumentException("Only supports ground keys")
        }

        return groups.put(key, value)
    }

    override fun upgradeFor(key: Unification): UnificationGroupingStrategy<T> {
        return ListUnificationGroupingStrategy(randomVariableScope, this)
    }

    override fun clear() {
        groups.clear()
    }

    override fun remove(key: Unification) = groups.remove(key)

    override fun iterator(): MutableIterator<Map.Entry<Unification, T>> = groups.iterator()

    private val Unification.isGround: Boolean get() = entries.all { (_, value) -> value.isGround }
}

private class ListUnificationGroupingStrategy<T : Any>(
    private val randomVariableScope: RandomVariableScope,
    initializeFrom: UnificationGroupingStrategy<T>? = null,
) : UnificationGroupingStrategy<T> {
    private val entries = ArrayList<Map.Entry<Unification, T>>(initializeFrom?.size ?: 10)

    init {
        initializeFrom?.forEach(entries::add)
    }

    override val size: Int get() = entries.size

    override fun iterator(): MutableIterator<Map.Entry<Unification, T>> = entries.iterator()

    override fun get(key: Unification): T? {
        val compactedKey = key.compacted()
        for ((vars, value) in entries) {
            if (vars.equalsStructurally(compactedKey, randomVariableScope)) {
                return value
            }
        }

        return null
    }

    override fun set(key: Unification, value: T): T? {
        val compactedKey = key.compacted()
        val iterator = entries.listIterator()
        while (iterator.hasNext()) {
            val currentEntry = iterator.next()
            if (currentEntry.key.equalsStructurally(compactedKey, randomVariableScope)) {
                iterator.set(MapEntry(currentEntry.key, value))
                return currentEntry.value
            }
        }

        // no existing entry matched
        entries.add(MapEntry(key, value))
        return null
    }

    override fun upgradeFor(key: Unification): UnificationGroupingStrategy<T> {
        throw NotImplementedError("This strategy can handle all keys")
    }

    override fun clear() {
        entries.clear()
    }

    override fun remove(key: Unification): T? {
        val compactedKey = key.compacted()
        val iterator = entries.listIterator()
        while (iterator.hasNext()) {
            val currentEntry = iterator.next()
            if (currentEntry.key.equalsStructurally(compactedKey, randomVariableScope)) {
                iterator.remove()
                return currentEntry.value
            }
        }

        return null
    }
}

private class MapEntry<K, V>(
    override val key: K,
    override val value: V,
) : Map.Entry<K, V>