package com.github.prologdb.async

import java.util.concurrent.CancellationException
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import kotlin.coroutines.experimental.*

class WorkableFutureImpl<T>(code: suspend WorkableFutureBuilder.() -> T) : WorkableFuture<T> {

    private val onComplete = object : Continuation<T> {
        override val context: CoroutineContext = EmptyCoroutineContext

        override fun resume(value: T) {
            synchronized(mutex) {
                if (state == State.RUNNING) {
                    result = value!!
                    state = State.COMPLETED
                }
            }
        }

        override fun resumeWithException(exception: Throwable) {
            synchronized(mutex) {
                if (state == State.RUNNING) {
                    error = exception
                    state = State.COMPLETED
                }
            }
        }
    }

    private val mutex = Any()

    @Volatile
    private var result: T? = null
    @Volatile
    private var error: Throwable? = null

    @Volatile
    private var state: State = State.RUNNING

    @Volatile
    private var currentWaitingFuture: Future<*>? = null

    override fun isDone(): Boolean = state == State.COMPLETED || state == State.CANCELLED

    override fun isCancelled(): Boolean = state == State.CANCELLED

    override fun step(): Boolean {
        synchronized(mutex) {
            when (state) {
                State.RUNNING -> continuation.resume(Unit)
                State.WAITING_ON_FUTURE -> {
                    val future = currentWaitingFuture!!

                    if (future.isDone) {
                        state = State.RUNNING
                        currentWaitingFuture = null
                        continuation.resume(Unit)
                    }
                    else if (future is WorkableFuture) {
                        if (future.step()) {
                            state = State.RUNNING
                            currentWaitingFuture = null
                            continuation.resume(Unit)
                        }
                    }
                }
                State.COMPLETED, State.CANCELLED -> { }
            }
        }

        return isDone
    }

    override fun get(): T {
        do {
            step()
            when(state) {
                State.COMPLETED -> return result ?: throw error!!
                State.CANCELLED -> throw error as CancellationException
                State.RUNNING -> { /* do nothing, keep looping */ }
                State.WAITING_ON_FUTURE -> {
                    try {
                        currentWaitingFuture!!.get()
                    }
                    catch (handleLater: Exception) {
                        /* calling get() will complete the future
                         * the next iteration will call step()
                         * step() will detect that the future is
                         * completed and update the state accordingly
                         * the CANCELLED branch of this when() will
                         * pick that up
                         */
                    }
                }
            }
        } while (true)
    }

    override fun get(timeout: Long, unit: TimeUnit?): T {
        TODO("Implement only when needed.")
    }

    override fun cancel(mayInterruptIfRunning: Boolean): Boolean {
        synchronized(mutex) {
            when (state) {
                State.RUNNING, State.WAITING_ON_FUTURE -> {
                    state = State.CANCELLED
                    error = CancellationException()
                    currentWaitingFuture?.cancel(true)
                    currentWaitingFuture = null

                    return true
                }
                State.COMPLETED, State.CANCELLED -> {
                    return false
                }
            }
        }
    }

    private val Builder = object : WorkableFutureBuilder {
        override suspend fun <E> await(future: Future<E>): E {
            synchronized(mutex) {
                if (state == State.CANCELLED) {
                    // this has been cancelled, shut it down right here
                    suspendCoroutine<Unit> { /* not picking up the continuation aborts the coroutine. */ }
                    throw Exception("This should never have been thrown")
                }

                if (state != State.RUNNING) {
                    throw IllegalStateException("Future is in state $state, cannot wait for a future.")
                }

                if (future.isDone) {
                    return try {
                        future.get()
                    }
                    catch (ex: ExecutionException) {
                        throw ex.cause ?: ex
                    }
                }

                currentWaitingFuture = future
                state = State.WAITING_ON_FUTURE
            }

            suspendCoroutine<Any> { continuation = it }
            return try {
                future.get()
            }
            catch (ex: ExecutionException) {
                throw ex.cause ?: ex
            }
        }
    }

    @Volatile
    private var continuation: Continuation<Unit> = code.createCoroutine(Builder, onComplete)

    private enum class State {
        RUNNING,
        COMPLETED,
        CANCELLED,
        WAITING_ON_FUTURE
    }
}