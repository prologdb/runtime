package com.github.prologdb.async

import java.util.concurrent.CancellationException
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import java.util.concurrent.LinkedBlockingQueue
import kotlin.coroutines.experimental.*

/**
 * Implements [WorkableLazySequence] with the following characteristics:
 * * Calling [step] when [state] is [WorkableLazySequence.State.RESULTS_AVAILABLE] attempts
 *   to do more work. While waiting for a sub-sequence (see [WorkableLazySequenceBuilder.yieldAll]),
 *   the behaviour depends on that sequence.
 */
class WorkableLazySequenceImpl<T>(code: suspend WorkableLazySequenceBuilder<T>.() -> Unit) : WorkableLazySequence<T> {

    /**
     * The sequence itself is always working. Results are cached if
     * [step] is called more often than necessary for one result.
     */
    private enum class InnerState {
        /**
         * Between steps that do not yield nor declare a future to wait for
         */
        RUNNING,

        /**
         * The coroutine has called [WorkableLazySequenceBuilder.yieldAll].
         * Calculations and result access is to be done on the subsequence.
         */
        SUBSEQUENCE,

        /**
         * Currently waiting for a future. In this case, the [continuation]
         * needs to be invoked with the future return value (typing is
         * erased out of necessity)
         */
        WAITING_ON_FUTURE,

        DEPLETED,

        FAILED
    }

    /**
     * When [step] finds a new solution, it is appended to this queue.
     * This way, invocations of [step] can advance the progress; consumption
     * of the results can follow another pace.
     */
    private val queuedResults = LinkedBlockingQueue<T>()

    @Volatile private var innerState: InnerState = InnerState.RUNNING

    /* These are important in state SUBSEQUENCE */
    @Volatile private var currentSubSequenceSimple: LazySequence<T>? = null
    @Volatile private var currentSubSequenceWorkable: WorkableLazySequence<T>? = null
    @Volatile private var currentSubSequenceIsWorkable: Boolean = false

    /* These are important in state WAITING_ON_FUTURE */
    @Volatile private var currentWaitingFuture: Future<*>? = null

    /* These are important in state FAILED */
    @Volatile private var error: Throwable? = null

    override val state: WorkableLazySequence.State
        get() = if (queuedResults.isNotEmpty()) WorkableLazySequence.State.RESULTS_AVAILABLE else when (innerState) {
            InnerState.RUNNING,
            InnerState.SUBSEQUENCE,
            InnerState.WAITING_ON_FUTURE -> WorkableLazySequence.State.PENDING
            InnerState.DEPLETED -> WorkableLazySequence.State.DEPLETED
            InnerState.FAILED -> WorkableLazySequence.State.FAILED
        }

    private val onComplete = object : Continuation<Unit> {
        override val context: CoroutineContext = EmptyCoroutineContext

        override fun resume(value: Unit) {
            innerState = InnerState.DEPLETED
        }

        override fun resumeWithException(exception: Throwable) {
            error = exception
            innerState = InnerState.FAILED
        }
    }

    private val builder = object : WorkableLazySequenceBuilder<T> {
        override suspend fun <E> await(future: Future<E>): E {
            currentWaitingFuture = future
            innerState = InnerState.WAITING_ON_FUTURE

            suspendCoroutine<Any> { continuation = it }

            return try {
                future.get()
            }
            catch (ex: ExecutionException) {
                throw ex.cause ?: ex
            }
        }

        override suspend fun yield(result: T) {
            queuedResults.add(result)
            innerState = InnerState.RUNNING

            suspendCoroutine<Any> { continuation = it }
        }

        override suspend fun yieldAll(results: LazySequence<T>) {
            if (results is WorkableLazySequence) {
                currentSubSequenceIsWorkable = true
                currentSubSequenceSimple = null
                currentSubSequenceWorkable = results
            } else {
                currentSubSequenceIsWorkable = false
                currentSubSequenceWorkable = null
                currentSubSequenceSimple = results
            }

            suspendCoroutine<Any> { continuation = it }
        }
    }

    private var continuation: Continuation<Any> = code.createCoroutine(builder, onComplete) as Continuation<Any>

    private val stepMutex = Any()

    override fun step(): WorkableLazySequence.State {
        synchronized(stepMutex) {
            when (innerState) {
                InnerState.FAILED,
                InnerState.DEPLETED -> return state

                InnerState.WAITING_ON_FUTURE -> {
                    val future = currentWaitingFuture!!
                    if (future.isDone || future.isCancelled) {
                        val result = try {
                            future.get()
                        } catch (ex: Throwable) {
                            error = ex
                            innerState = InnerState.FAILED
                            return state
                        }

                        innerState = InnerState.RUNNING
                        currentWaitingFuture = null

                        continuation.resume(result)
                    }
                    // else -> future not yet done, continue to wait
                }

                InnerState.RUNNING -> continuation.resume(Unit)

                InnerState.SUBSEQUENCE -> {
                    if (currentSubSequenceIsWorkable) {
                        val subSeq = currentSubSequenceWorkable!!
                        var subState = subSeq.step()

                        if (subState == WorkableLazySequence.State.DEPLETED) {
                            innerState = InnerState.RUNNING
                            currentSubSequenceWorkable = null
                            continuation.resume(Unit)
                        }
                        else
                        {
                            while (subState == WorkableLazySequence.State.RESULTS_AVAILABLE) {
                                queuedResults.add(subSeq.tryAdvance() ?: break)
                                subState = subSeq.state
                            }

                            when (subState) {
                                WorkableLazySequence.State.DEPLETED -> {
                                    innerState = InnerState.RUNNING
                                    currentSubSequenceWorkable = null
                                }
                                WorkableLazySequence.State.FAILED -> {
                                    innerState = InnerState.FAILED
                                    currentSubSequenceWorkable = null
                                }
                                else -> { /* all good, go on */ }
                            }
                        }
                    }
                    else {
                        val subResult = currentSubSequenceSimple!!.tryAdvance()
                        if (subResult != null) {
                            queuedResults.add(subResult)
                        } else {
                            // sub sequence depleted
                            innerState = InnerState.RUNNING
                            currentSubSequenceSimple = null
                        }
                    }
                }
            }
            Unit
        }

        return state
    }

    override fun tryAdvance(): T? {
        while (queuedResults.isEmpty()) {
            when (innerState) {
                InnerState.DEPLETED -> return null
                InnerState.FAILED -> throw error ?: throw IllegalStateException()
                InnerState.WAITING_ON_FUTURE -> {
                    try {
                        currentWaitingFuture!!.get()
                    }
                    catch (handledInStep: Throwable) {}
                    step()
                }
                else -> step()
            }
        }

        // catch race condition between loop condition and now
        return queuedResults.poll() ?: tryAdvance()
    }

    override fun close() {
        if (innerState == InnerState.DEPLETED || innerState == InnerState.FAILED) return

        synchronized(stepMutex) {
            innerState = InnerState.DEPLETED
            currentSubSequenceWorkable?.close()
            currentSubSequenceSimple?.close()
            currentWaitingFuture?.cancel(true)
        }
    }
}

interface WorkableLazySequenceBuilder<T> {
    /**
     * Suspends this coroutine until the given future is present.
     *
     * If the given future is already done or cancelled, returns/throws
     * immediately without suspending the coroutine.
     *
     * @return the futures value
     * @throws Exception Forwarded from the [Future], including [CancellationException]
     */
    suspend fun <E> await(future: Future<E>): E

    /**
     * Yields the given object as one result to the lazy sequence
     */
    suspend fun yield(result: T)

    /**
     * Yields all results of the given lazy sequence to this
     * lazy sequence.
     */
    suspend fun yieldAll(results: LazySequence<T>)
}