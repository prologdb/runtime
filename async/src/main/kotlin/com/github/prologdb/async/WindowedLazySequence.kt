package com.github.prologdb.async

import java.util.LinkedList

/**
 * @see [windowed]
 */
class WindowedLazySequence<T : Any> constructor(
    private val base: LazySequence<T>,
    val size: Int,
    val step: Int = 1,
    val partialWindows: Boolean = false,
) : LazySequence<List<T>> {
    init {
        require(size >= 1)
        require(step >= 1)
    }

    override val principal = base.principal

    private val window = LinkedList<T>()
    /* the number of elements that need to be skipped from base to achieve the step size */
    private var nToSkip = 0

    override fun step(): LazySequence.State {
        return when (val localState = state) {
            LazySequence.State.RESULTS_AVAILABLE,
            LazySequence.State.DEPLETED,
            LazySequence.State.FAILED -> localState
            LazySequence.State.PENDING -> {
                base.step()
                state
            }
        }
    }

    override val state: LazySequence.State
        get() {
            if (nToSkip == 0 && window.size >= size) {
                return LazySequence.State.RESULTS_AVAILABLE
            }

            return when(base.state) {
                LazySequence.State.RESULTS_AVAILABLE -> {
                    while (base.state == LazySequence.State.RESULTS_AVAILABLE && window.size < size) {
                        val baseElement = base.tryAdvance()!!
                        if (nToSkip > 0) {
                            nToSkip--
                        } else {
                            window.addLast(baseElement)
                        }
                    }
                    state
                }
                LazySequence.State.DEPLETED -> when {
                    window.isEmpty() -> LazySequence.State.DEPLETED
                    partialWindows -> LazySequence.State.RESULTS_AVAILABLE
                    else -> LazySequence.State.DEPLETED
                }
                LazySequence.State.FAILED -> LazySequence.State.FAILED
                LazySequence.State.PENDING -> LazySequence.State.PENDING
            }
        }

    override fun tryAdvance(): List<T>? {
        return when (state) {
            LazySequence.State.RESULTS_AVAILABLE -> {
                val element = ArrayList(window)

                val nToRemove = step.coerceAtMost(window.size)
                nToSkip = step - nToRemove
                repeat(nToRemove) {
                    window.removeFirst()
                }

                element
            }
            LazySequence.State.FAILED -> {
                base.tryAdvance()
                throw IllegalStateException("unreachable, base.tryAdvance() should have thrown")
            }
            LazySequence.State.DEPLETED -> null
            LazySequence.State.PENDING -> {
                while(state == LazySequence.State.PENDING) {
                    step()
                }
                tryAdvance()
            }
        }
    }

    override fun close() {
        base.close()
        window.clear()
    }
}