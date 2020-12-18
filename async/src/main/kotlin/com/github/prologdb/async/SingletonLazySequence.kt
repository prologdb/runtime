package com.github.prologdb.async

internal class SingletonLazySequence<T : Any>(
    private val element: T,
    override val principal: Principal
) : LazySequence<T> {
    private var consumed = false

    override val state: LazySequence.State
        get() = if (consumed) LazySequence.State.DEPLETED else LazySequence.State.RESULTS_AVAILABLE

    override fun step() = state

    override fun tryAdvance(): T? = if (consumed) null else { consumed = true; element }

    override fun close() {
        consumed = true
    }
}