package com.github.prologdb.async

internal class RandomAccessListLazySequence<T : Any, L : List<T>>(
    private var elements: L?,
    override val principal: Principal
) : LazySequence<T> {
    init {
        require(elements is RandomAccess)
    }

    private var index = 0
    private var closed = false

    override fun step(): LazySequence.State = state

    override val state: LazySequence.State
        get() = if (closed || index >= elements!!.size) LazySequence.State.DEPLETED else LazySequence.State.RESULTS_AVAILABLE

    override fun tryAdvance(): T? {
        val es = elements
        if (closed || index >= es!!.size) return null
        return es[index++]
    }

    override fun close() {
        closed = true
        elements = null
    }
}