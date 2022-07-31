package com.github.prologdb.async

import java.util.concurrent.LinkedBlockingDeque

class MappedLazySequence<T : Any, M : Any>(
    private val base: LazySequence<T>,
    private val mapper: (T) -> M
) : LazySequence<M> {
    private var closed = false

    private var mapperException: Throwable? = null

    override val principal = base.principal

    private val cachedResults = LinkedBlockingDeque<M>()

    private val stepMutex = Any()

    override val state: LazySequence.State
        get() = when {
            closed -> LazySequence.State.DEPLETED
            cachedResults.isNotEmpty() -> LazySequence.State.RESULTS_AVAILABLE
            mapperException != null -> LazySequence.State.FAILED
            else -> when (base.state) {
                LazySequence.State.FAILED -> LazySequence.State.FAILED
                LazySequence.State.DEPLETED -> LazySequence.State.DEPLETED
                LazySequence.State.PENDING -> LazySequence.State.PENDING
                LazySequence.State.RESULTS_AVAILABLE -> LazySequence.State.PENDING
            }
        }

    override fun step(): LazySequence.State {
        synchronized(stepMutex) {
            var baseState = base.step()
            while (baseState == LazySequence.State.RESULTS_AVAILABLE) {
                val baseResult = base.tryAdvance() ?: break
                baseState = base.state

                val mappedResult = try {
                    mapper(baseResult)
                } catch (mapperEx: Throwable) {
                    mapperException = mapperEx
                    break
                }
                cachedResults.add(mappedResult)
            }

            return state
        }
    }

    override tailrec fun tryAdvance(): M? {
        while (state == LazySequence.State.RESULTS_AVAILABLE) {
            cachedResults.poll()?.let { return it }
        }

        return when (step()) {
            LazySequence.State.RESULTS_AVAILABLE -> tryAdvance()
            LazySequence.State.DEPLETED -> null
            LazySequence.State.FAILED -> {
                if (mapperException != null) {
                    throw mapperException!!
                } else {
                    base.tryAdvance()
                    error("base.tryAdvance() should have thrown")
                }
            }
            LazySequence.State.PENDING -> tryAdvance()
        }
    }

    override fun close() {
        if (closed) return
        closed = true

        cachedResults.clear()
        base.close()
    }
}
