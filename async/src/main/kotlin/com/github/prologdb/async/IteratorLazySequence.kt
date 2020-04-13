package com.github.prologdb.async

internal class IteratorLazySequence<T : Any>(
    iterable: Iterable<T>,
    override val principal: Principal
) : LazySequence<T> {
    private val iterator = iterable.iterator()

    override fun step() = state
    override val state: LazySequence.State
        get() = when {
            iterator.hasNext() -> LazySequence.State.RESULTS_AVAILABLE
            else -> LazySequence.State.DEPLETED
        }

    override fun tryAdvance(): T? {
        return if (iterator.hasNext()) iterator.next() else null
    }

    override fun close() {
       while (iterator.hasNext()) {
           iterator.next()
       }
    }
}