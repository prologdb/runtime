package com.github.prologdb.async

import com.github.prologdb.async.LazySequence.State.*

class SkippingLazySequence<T : Any>(val base: LazySequence<T>, val skipN: Long) : LazySequence<T> {
    init {
        require(skipN >= 0)
    }

    private var nSkipped: Long = 0
    private val stepAndAdvanceMutexDuringLimiting = Any()

    override val principal = base.principal

    override fun step(): LazySequence.State {
        if (nSkipped >= skipN) {
            return base.step()
        }

        synchronized(stepAndAdvanceMutexDuringLimiting) {
            val baseStateAfterStep = base.step()

            if (nSkipped >= skipN) {
                return baseStateAfterStep
            }

            while (base.state == RESULTS_AVAILABLE && nSkipped < skipN) {
                check(base.tryAdvance() != null)
                nSkipped++
            }

            return base.state
        }
    }

    override val state: LazySequence.State get() {
        if (nSkipped >= skipN) {
            return base.state
        }

        synchronized(stepAndAdvanceMutexDuringLimiting) {
            if (nSkipped >= skipN) {
                return base.state
            }

            return when(base.state) {
                RESULTS_AVAILABLE -> PENDING
                PENDING -> PENDING
                DEPLETED -> DEPLETED
                FAILED -> FAILED
            }
        }
    }

    override fun tryAdvance(): T? {
        if (nSkipped >= skipN) {
            return base.tryAdvance()
        }

        synchronized(stepAndAdvanceMutexDuringLimiting) {
            if (nSkipped >= skipN) {
                return base.tryAdvance()
            }

            while (nSkipped < skipN) {
                base.tryAdvance()
                nSkipped++
            }

            return base.tryAdvance()
        }
    }

    override fun close() {
        base.close()
    }
}