package com.github.prologdb.async

internal class ArrayLazySequence<T : Any>(
    private val elements: Array<out T>,
    override val principal: Principal
) : LazySequence<T> {
    private var index = 0
    private var closed = false

    override fun step(): LazySequence.State = state

    override val state: LazySequence.State
        get() = if (closed || index >= elements.size) LazySequence.State.DEPLETED else LazySequence.State.RESULTS_AVAILABLE

    override fun tryAdvance(): T? {
        if (closed || index >= elements.size) return null
        return elements[index++]
    }

    override fun close() {
        closed = true
    }
}