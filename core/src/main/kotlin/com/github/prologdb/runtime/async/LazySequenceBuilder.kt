package com.github.prologdb.runtime.async

import kotlin.coroutines.experimental.*

@RestrictsSuspension
class LazySequenceBuilder<T>(searchFun: suspend LazySequenceBuilder<T>.() -> Unit) {

    private val sequenceImpl = SequenceImpl<T>()
    private var continuation: Continuation<Unit> = searchFun.createCoroutine(this, sequenceImpl.onComplete)

    val sequence: LazySequence<T> = sequenceImpl

    private enum class SequenceState {
        /** State after creation, no element has been found yet */
        INITIAL,

        /** An element has been found and is available to be returned from tryAdvance() */
        ELEMENT_AVAILABLE,

        /**
         * A lazysequence of elements has been found and tryAdvance() can return its elements first before resuming
         * the coroutine
         */
        SUBSEQUENCE_AVAILABLE,

        /** A subsequence has been depleted and the original coroutine can be resumed */
        AFTER_SUBSEQUENCE,

        /** Sequence is completed, no more elements available */
        COMPLETED,

        /** Sequence was aborted because of an exception; the exception is stored in [SequenceImpl.exception] */
        ERRORED
    }

    private inner class SequenceImpl<T> : LazySequence<T> {

        var state: SequenceState = SequenceState.INITIAL

        val isCompleted: Boolean
            get() = state == SequenceState.COMPLETED || state == SequenceState.ERRORED

        var currentElement: T? = null
        var currentSubSequence: LazySequence<out T>? = null
        var exception: Throwable? = null

        /** used to synchronize all mutations on this object */
        val mutex = Any()

        internal val onComplete: Continuation<Unit> = object : Continuation<Unit> {
            override val context: CoroutineContext = EmptyCoroutineContext

            override fun resume(value: Unit) {
                synchronized(mutex) {
                    if (isCompleted) throw IllegalStateException()

                    close()
                }
            }

            override fun resumeWithException(exception: Throwable) {
                synchronized(mutex) {
                    if (isCompleted) throw IllegalStateException()

                    this@SequenceImpl.exception = exception
                    state = SequenceState.ERRORED
                    currentElement = null
                    currentSubSequence = null
                }
            }
        }

        internal fun onElementAvailable(element: T) {
            synchronized(mutex) {
                currentElement = element
                state = SequenceState.ELEMENT_AVAILABLE
                currentSubSequence = null
            }
        }

        internal fun onSubSequenceAvailable(sequence: LazySequence<out T>) {
            synchronized(mutex) {
                currentSubSequence = sequence
                state = SequenceState.SUBSEQUENCE_AVAILABLE
                currentElement = null
            }
        }

        override fun tryAdvance(): T? {
            synchronized(mutex) {
                if (isCompleted) return null

                if (state == SequenceState.SUBSEQUENCE_AVAILABLE) {
                    val result = currentSubSequence!!.tryAdvance()
                    if (result != null) {
                        return result
                    }

                    state = SequenceState.AFTER_SUBSEQUENCE
                    currentSubSequence = null
                }

                continuation.resume(Unit)

                return when (state) {
                    SequenceState.ELEMENT_AVAILABLE     -> currentElement
                    SequenceState.COMPLETED             -> null
                    SequenceState.SUBSEQUENCE_AVAILABLE -> tryAdvance()

                    SequenceState.ERRORED               -> throw exception ?: IllegalStateException()
                    SequenceState.AFTER_SUBSEQUENCE     -> throw IllegalStateException()
                    SequenceState.INITIAL               -> throw IllegalStateException()
                }
            }
        }

        override fun close() {
            synchronized(mutex) {
                state = SequenceState.COMPLETED
                currentElement = null
                currentSubSequence = null
            }
        }
    }

    suspend fun yield(element: T) {
        sequenceImpl.onElementAvailable(element)
        suspendCoroutine<Unit> { continuation = it }
    }

    suspend fun yieldAll(sequence: LazySequence<out T>) {
        sequenceImpl.onSubSequenceAvailable(sequence)
        suspendCoroutine<Unit> { continuation = it }
    }
}

fun <T> buildLazySequence(searchFun: suspend LazySequenceBuilder<T>.() -> Unit): LazySequence<T>
    = LazySequenceBuilder(searchFun).sequence