package com.github.prologdb.async

class DistinctLazySequence<T, K>(
    base: LazySequence<out T>,
    selector: (T) -> K
) : LazySequence<T> {
    override val principal = base.principal

    private val seenKeys: MutableSet<K> = HashSet()

    private val wrapped = FilteredLazySequence(base) { value ->
        val key = selector(value)
        seenKeys.add(key) // if add returns false it was already there so not unique
    }

    override fun step() = wrapped.step()

    override val state
        get() = wrapped.state

    override fun tryAdvance() = wrapped.tryAdvance()

    override fun close() {
        wrapped.close()
        seenKeys.clear()
    }
}