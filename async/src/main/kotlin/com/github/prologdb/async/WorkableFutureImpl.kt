package com.github.prologdb.async

import java.util.LinkedList
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import kotlin.coroutines.Continuation
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.coroutines.cancellation.CancellationException
import kotlin.coroutines.createCoroutine
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlin.coroutines.suspendCoroutine

class WorkableFutureImpl<T>(override val principal: Any, code: suspend WorkableFutureBuilder.() -> T) : WorkableFuture<T> {

    private val onComplete = object : Continuation<T> {
        override val context: CoroutineContext = EmptyCoroutineContext

        override fun resumeWith(result: Result<T>) {
            try {
                synchronized(mutex) {
                    if (result.isSuccess) {
                        this@WorkableFutureImpl.result = result.getOrNull()
                    } else {
                        error = result.exceptionOrNull()!!
                    }
                }
            } finally {
                state = State.COMPLETED
                tearDown()
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

    @Volatile
    private var currentFoldingSequence: LazySequence<*>? = null
    @Volatile
    private var currentFoldingCarry: Any? = null
    @Volatile
    private var currentFoldingAccumulator: ((Any, Any) -> Any)? = null

    /**
     * Teardown code, in the reverse order as it was added using [WorkableFutureBuilder.finally]; see
     * [LinkedList.addFirst].
     * Is set on the first call to [WorkableFutureBuilder.finally] because it is expected
     * that most futures will not have teardown logic.
     */
    private var teardownLogic: LinkedList<() -> Any?>? = null
    @Volatile
    private var tornDown: Boolean = false

    override fun isDone(): Boolean = state == State.COMPLETED

    override fun isCancelled(): Boolean = false

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
                State.FOLDING_SEQUENCE -> {
                    val sequence = currentFoldingSequence!!
                    var seqState = sequence.state
                    if (seqState == LazySequence.State.PENDING) {
                        seqState = sequence.step()
                    }

                    when (seqState) {
                        LazySequence.State.DEPLETED -> {
                            // great, accumulation is done
                            val result = currentFoldingCarry!!

                            state = State.RUNNING
                            sequence.close()
                            currentFoldingSequence = null
                            currentFoldingCarry = null
                            currentFoldingCarry = null

                            continuation.resume(result)
                        }
                        LazySequence.State.RESULTS_AVAILABLE -> {
                            val element = sequence.tryAdvance()!!
                            currentFoldingCarry = currentFoldingAccumulator!!(currentFoldingCarry!!, element)
                        }
                        LazySequence.State.FAILED -> {
                            val exception = try {
                                sequence.tryAdvance()
                                null
                            } catch (ex: Throwable) {
                                ex
                            } ?: throw IllegalStateException("Subsequence failed but did not throw the execption when tryAdvance() was called")

                            state = State.RUNNING
                            currentFoldingCarry = null
                            currentFoldingSequence = null
                            currentFoldingAccumulator = null

                            continuation.resumeWithException(exception)
                        }
                        LazySequence.State.PENDING -> { /* cannot do anything */ }
                    }
                }
                State.COMPLETED -> { }
            }
        }

        return isDone
    }

    override fun get(): T {
        do {
            step()
            when(state) {
                State.RUNNING -> { /* do nothing, keep looping */
                }
                State.FOLDING_SEQUENCE -> { /* do nothing, keep looping */
                }
                State.COMPLETED -> return result ?: throw error!!
                State.WAITING_ON_FUTURE -> {
                    try {
                        currentWaitingFuture!!.get()
                    }
                    catch (handleLater: Exception) {
                        /* calling get() will complete the future
                         * the next iteration will call step()
                         * step() will detect that the future is
                         * completed and update the state accordingly
                         * the COMPLETED branch of this when() will
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
        fun continueWithCancellationException() {
            state = State.RUNNING
            continuation.resumeWithException(CancellationException().apply {
                fillInStackTrace()
            })
        }

        synchronized(mutex) {
            when (state) {
                State.WAITING_ON_FUTURE,
                State.FOLDING_SEQUENCE,
                State.RUNNING -> {
                    currentWaitingFuture?.let { future ->
                        if (!future.isDone) {
                            if (!future.cancel(mayInterruptIfRunning)) {
                                throw WorkableFutureNotCancellableException("Waiting on nested future which is not done and cannot be cancelled.")
                            }
                        }
                    }
                    currentWaitingFuture = null

                    currentFoldingSequence?.close()
                    currentFoldingSequence = null

                    continueWithCancellationException()
                    if (state != State.COMPLETED || error == null || error!! !is CancellationException) {
                        throw WorkableFutureNotCancellableException()
                    }
                    return true
                }
                State.COMPLETED -> {
                    return false
                }
            }
        }
    }

    private fun tearDown() {
        if (tornDown) {
            throw IllegalStateException("TearDown already executed.")
        }
        tornDown = true

        teardownLogic?.let {
            var errors: ArrayList<Throwable>? = null

            it.forEach { tearDown ->
                try {
                    tearDown()
                }
                catch (ex: Throwable) {
                    if (errors == null) errors = ArrayList()
                    errors!!.add(ex)
                }
            }

            if (errors != null && errors!!.isNotEmpty()) {
                val topEx = RuntimeException("TearDown code errored after cancel", errors!!.first())
                val errorIt = errors!!.iterator()
                errorIt.next() // skip first, is set as cause
                errorIt.forEachRemaining(topEx::addSuppressed)

                throw topEx
            }
        }
    }

    private val Builder = object : WorkableFutureBuilder {
        override val principal = this@WorkableFutureImpl.principal

        override suspend fun <E> await(future: Future<E>): E {
            if (future is WorkableFuture) {
                PrincipalConflictException.requireCompatible(principal, future.principal)
            }

            synchronized(mutex) {
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

        override suspend fun <E : Any, C> foldRemaining(sequence: LazySequence<E>, initial: C, accumulator: (C, E) -> C): C {
            PrincipalConflictException.requireCompatible(principal, sequence.principal)

            synchronized(mutex) {
                if (state != State.RUNNING) {
                    throw IllegalStateException("Future is in state $state, cannot fold a sequence")
                }

                currentFoldingSequence = sequence
                currentFoldingCarry = initial
                currentFoldingAccumulator = accumulator as (Any, Any) -> Any
                state = State.FOLDING_SEQUENCE
            }

            return suspendCoroutine { continuation = it as Continuation<Any> }
        }

        override fun finally(code: () -> Any?) {
            if (teardownLogic == null) {
                teardownLogic = LinkedList()
            }
            teardownLogic!!.addFirst(code)
        }
    }

    @Volatile
    private var continuation: Continuation<Any> = code.createCoroutine(Builder, onComplete) as Continuation<Any>

    private enum class State {
        RUNNING,
        COMPLETED,
        WAITING_ON_FUTURE,
        FOLDING_SEQUENCE
    }
}
