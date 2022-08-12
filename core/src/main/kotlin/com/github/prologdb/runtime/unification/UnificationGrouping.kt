package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.CircularTermException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Variable

/**
 * Groups [T]s by a [Unification]. Optimizes with [Any.hashCode]  as long as the [Unification]s
 * are fully ground (see [Term.isGround]). When a non-ground [Unification] is introduced, all
 * operations slow down to `O(n)` performance.
 *
 * TODO: thread-safety
 */
interface UnificationGrouping<T : Any> : Iterable<Map.Entry<VariableBucket, T>> {
    operator fun get(key: VariableBucket): T?
    operator fun set(key: VariableBucket, value: T)
    val size: Int

    companion object {
        operator fun <T : Any> invoke(randomVariableScope: RandomVariableScope): UnificationGrouping<T> {
            return StrategyUnificationGrouping(HashMapUnificationGroupingStrategy(randomVariableScope))
        }
    }
}

private class StrategyUnificationGrouping<T : Any>(var strategy: UnificationGroupingStrategy<T>) : UnificationGrouping<T> {

    override fun get(key: VariableBucket): T? = strategy.get(key)

    override fun set(key: VariableBucket, value: T) {
        if (!strategy.trySet(key, value)) {
            strategy = strategy.upgradeFor(key)
            check(strategy.trySet(key, value))
        }
    }

    override fun iterator(): Iterator<Map.Entry<VariableBucket, T>> = strategy.iterator()

    override val size: Int get() = strategy.size
}

private interface UnificationGroupingStrategy<T> : Iterable<Map.Entry<VariableBucket, T>> {
    fun get(key: VariableBucket): T?

    /**
     * Attempts to set the given value.
     * @return whether the value was set; false if this strategy cannot handle the [key].
     * In that case, invoke [upgradeFor].
     */
    fun trySet(key: VariableBucket, value: T): Boolean

    fun upgradeFor(key: VariableBucket): UnificationGroupingStrategy<T>

    val size: Int
}

private class HashMapUnificationGroupingStrategy<T : Any>(
    private val randomVariableScope: RandomVariableScope,
) : UnificationGroupingStrategy<T> {
    private val groups = HashMap<VariableBucket, T>()

    override val size: Int get() = groups.size

    override fun get(key: VariableBucket): T? = groups[key]

    override fun trySet(key: VariableBucket, value: T): Boolean {
        val compactedKey = key.compact(randomVariableScope)
        if (!compactedKey.isGround) {
            return false
        }

        groups[key] = value

        return true
    }

    override fun upgradeFor(key: VariableBucket): UnificationGroupingStrategy<T> {
        return ListUnificationGroupingStrategy(randomVariableScope, this)
    }

    override fun iterator(): Iterator<Map.Entry<VariableBucket, T>> = groups.iterator()

    private val VariableBucket.isGround: Boolean get() = values.all { (_, value) -> value.isGround }
}

private class ListUnificationGroupingStrategy<T : Any>(
    private val randomVariableScope: RandomVariableScope,
    initializeFrom: UnificationGroupingStrategy<T>? = null,
) : UnificationGroupingStrategy<T> {
    private val entries = ArrayList<Map.Entry<VariableBucket, T>>(initializeFrom?.size ?: 10)

    init {
        initializeFrom?.forEach(entries::add)
    }

    override val size: Int get() = entries.size

    override fun iterator(): Iterator<Map.Entry<VariableBucket, T>> = entries.iterator()

    override fun get(key: VariableBucket): T? {
        val compactedKey = key.compact(randomVariableScope)
        for ((vars, value) in entries) {
            if (vars.equalsIgnoringUnboundVariables(compactedKey, randomVariableScope)) {
                return value
            }
        }

        return null
    }

    override fun trySet(key: VariableBucket, value: T): Boolean {
        val compactedKey = key.compact(randomVariableScope)
        val iterator = entries.listIterator()
        while (iterator.hasNext()) {
            val currentEntry = iterator.next()
            if (currentEntry.key.equalsIgnoringUnboundVariables(compactedKey, randomVariableScope)) {
                iterator.set(MapEntry(currentEntry.key, value))
                return true
            }
        }

        // no existing entry matched
        entries.add(MapEntry(key, value))
        return true
    }

    override fun upgradeFor(key: VariableBucket): UnificationGroupingStrategy<T> {
        throw NotImplementedError("This strategy can handle all keys")
    }
}

private fun VariableBucket.compact(randomVariableScope: RandomVariableScope): VariableBucket {
    if (isEmpty) {
        return this
    }

    val sorted = try {
        sortForSubstitution()
    }
    catch (ex: CircularTermException) {
        return this
    }

    val result = sorted.first()
    if (sorted.size > 1) {
        for (next in sorted.subList(1, sorted.lastIndex)) {
            result.incorporate(next, randomVariableScope)
        }
    }

    return result
}

private fun VariableBucket.equalsIgnoringUnboundVariables(other: VariableBucket, randomVariableScope: RandomVariableScope): Boolean {
    val combined = try {
        combinedWith(other, randomVariableScope)
    }
    catch (ex: VariableDiscrepancyException) {
        return false
    }

    for ((combinedVariable, combinedValue) in combined.values) {
        if (!this.isInstantiated(combinedVariable) && combinedValue !is Variable) {
            return false
        }
        if (!other.isInstantiated(combinedVariable) && combinedValue !is Variable) {
            return false
        }
    }

    return true
}

private class MapEntry<K, V>(
    override val key: K,
    override val value: V,
) : Map.Entry<K, V>