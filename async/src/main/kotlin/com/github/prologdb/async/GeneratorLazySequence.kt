package com.github.prologdb.async

internal class GeneratorLazySequence<T : Any>(
    override val principal: Principal,
    private val generator: () -> T?
) : LazySequence<T> {
    var closed = false
    var cached: T? = null

    override fun step(): LazySequence.State {
        if (closed) return LazySequence.State.DEPLETED
        if (cached == null) {
            cached = generator()
            if (cached == null) {
                closed = true
                return LazySequence.State.DEPLETED
            }
        }

        return LazySequence.State.RESULTS_AVAILABLE
    }

    override val state: LazySequence.State
        get() = if (closed) LazySequence.State.DEPLETED else if (cached == null) LazySequence.State.PENDING else LazySequence.State.RESULTS_AVAILABLE

    override fun tryAdvance(): T? {
        if (closed) return null

        when (step()) {
            LazySequence.State.RESULTS_AVAILABLE -> {
                val result = cached
                cached = null
                return result
            }
            LazySequence.State.DEPLETED          -> return null
            else                                 -> throw IllegalStateException()
        }
    }

    override fun close() {
        closed = true
    }
}