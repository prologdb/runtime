package com.github.prologdb.runtime.lazysequence

class DistinctLazySequence<T, K>(
    private val base: LazySequence<out T>,
    private val selector: (T) -> K
) : LazySequence<T> {
    val keys: MutableSet<K> = HashSet()

    override fun tryAdvance(): T? {
        var baseValue: T
        var key: K

        do {
            baseValue = base.tryAdvance() ?: return null
            key = selector(baseValue)
        } while (!keys.add(key))

        return baseValue
    }

    override fun close() {
        base.close()
    }
}