package com.github.prologdb.async

import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import java.util.concurrent.LinkedBlockingQueue
import kotlin.coroutines.*

internal class LazySequenceImpl<T : Any>(override val principal: Any, code: suspend LazySequenceBuilder<T>.() -> T?) : LazySequence<T> {
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
         * The coroutine has called [LazySequenceBuilder.yieldAll].
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
    @Volatile private var currentSubSequence: LazySequence<T>? = null

    /* These are important in state WAITING_ON_FUTURE */
    @Volatile private var currentWaitingFuture: Future<*>? = null

    /* These are important in state FAILED */
    @Volatile private var error: Throwable? = null

    override val state: LazySequence.State
        get() = if (queuedResults.isNotEmpty()) LazySequence.State.RESULTS_AVAILABLE else when (innerState) {
            InnerState.RUNNING,
            InnerState.SUBSEQUENCE,
            InnerState.WAITING_ON_FUTURE -> LazySequence.State.PENDING
            InnerState.DEPLETED -> LazySequence.State.DEPLETED
            InnerState.FAILED -> LazySequence.State.FAILED
        }

    private val onComplete = object : Continuation<Unit> {
        override val context: CoroutineContext = EmptyCoroutineContext

        override fun resumeWith(result: Result<Unit>) {
            if (result.isSuccess) {
                innerState = InnerState.DEPLETED
            } else {
                error = result.exceptionOrNull()!!
                innerState = InnerState.FAILED
            }
        }
    }

    private val builder = object : LazySequenceBuilder<T> {
        override val principal = this@LazySequenceImpl.principal

        override suspend fun <E> await(future: Future<E>): E {
            if (future is WorkableFuture ) {
                PrincipalConflictException.requireCompatible(principal, future.principal)
            }

            if (!future.isDone) {
                currentWaitingFuture = future
                innerState = InnerState.WAITING_ON_FUTURE

                suspendCoroutine<Any> { continuation = it }
            }

            return try {
                future.get()
            }
            catch (ex: ExecutionException) {
                throw ex.cause ?: ex
            }
            catch (ex: Throwable) {
                throw ex
            }
        }

        override suspend fun yield(result: T) {
            queuedResults.add(result)
            innerState = InnerState.RUNNING

            suspendCoroutine<Any> { continuation = it }
        }

        override suspend fun yieldAll(results: Iterable<T>) {
            queuedResults.addAll(results)
            innerState = InnerState.RUNNING

            suspendCoroutine<Any> { continuation = it }
        }

        override suspend fun yieldAll(results: LazySequence<T>) {
            PrincipalConflictException.requireCompatible(principal, results.principal)

            currentSubSequence = results
            innerState = InnerState.SUBSEQUENCE

            suspendCoroutine<Any> { continuation = it }
        }
    }

    private var continuation: Continuation<Any> = code.createCoroutine(builder, onComplete) as Continuation<Any>

    private val stepMutex = Any()

    override fun step(): LazySequence.State {
        synchronized(stepMutex) {
            when (innerState) {
                InnerState.FAILED,
                InnerState.DEPLETED -> return state

                InnerState.WAITING_ON_FUTURE -> {
                    val future = currentWaitingFuture!!
                    if (future is WorkableFuture) {
                        future.step()
                    }

                    if (future.isDone) {
                        innerState = InnerState.RUNNING
                        currentWaitingFuture = null

                        continuation.resume(Unit)
                    }
                    // else -> future not yet done, continue to wait
                }

                InnerState.RUNNING -> continuation.resume(Unit)

                InnerState.SUBSEQUENCE -> {
                    val subSeq = currentSubSequence!!
                    var subState = subSeq.step()

                    if (subState == LazySequence.State.DEPLETED) {
                        innerState = InnerState.RUNNING
                        currentSubSequence = null
                        continuation.resume(Unit)
                    }
                    else
                    {
                        while (subState == LazySequence.State.RESULTS_AVAILABLE) {
                            val result = try {
                                subSeq.tryAdvance()
                            } catch (handledRightBelow: Throwable) { null }

                            queuedResults.add(result ?: break)
                            subState = subSeq.state
                        }

                        when (subState) {
                            LazySequence.State.DEPLETED -> {
                                innerState = InnerState.RUNNING
                                currentSubSequence = null
                            }
                            LazySequence.State.FAILED -> {
                                val ex = try {
                                    subSeq.tryAdvance()
                                    null
                                } catch (ex: Throwable) {
                                    ex
                                } ?: RuntimeException("Sub-Sequence reported state FAILED but tryAdvance() did not throw.")

                                continuation.resumeWithException(ex)
                            }
                            else -> { /* all good, go on */ }
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
        }

        currentSubSequence?.close()
        currentWaitingFuture?.cancel(true)
    }
}
