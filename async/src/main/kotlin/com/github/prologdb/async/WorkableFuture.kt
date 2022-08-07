package com.github.prologdb.async

import java.util.concurrent.CancellationException
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import kotlin.coroutines.RestrictsSuspension

/**
 * Resembles the work needed to compute a result which is split into multiple
 * steps each separated by a coroutine suspension. The [step] function can be
 * called repeatedly until a result is present or a error condition arises.
 */
interface WorkableFuture<T> : Future<T> {
    /**
     * The concurrency principal. Instead of [Thread], this is used to obtain locks and mutexes in the
     * name of the coroutine. Traditional [synchronized] blocks are still used to prevent multiple threads
     * from running the same coroutine simultaneously.
     */
    val principal: Principal

    /**
     * Performs CPU&memory bound work on this task returns when waiting for
     * I/O bound tasks (disk, memory, ...).
     *
     * If the result is already present, does nothing and returns `true`.
     *
     * This function can only be run by one thread at a time.
     * Implementations are inclined to use [synchronized] to achieve
     * this behaviour, [java.util.concurrent.locks.Lock]s are another
     * option, though.
     *
     * @return whether the result is available after this call
     */
    fun step(): Boolean

    /**
     * Attempts to cancel this future
     * @return true if cancellation was successful, false if the future is already completed.
     * However, as opposed to a default [Future], this method will not return false if running
     * but not cancellable. Instead, it will throw a [WorkableFutureNotCancellableException]
     */
    @Throws(WorkableFutureNotCancellableException::class)
    override fun cancel(mayInterruptIfRunning: Boolean): Boolean

    companion object {
        fun <T> completed(value: T): WorkableFuture<T> = object : WorkableFuture<T> {
            override val principal = IrrelevantPrincipal
            override fun step() = true
            override fun cancel(mayInterruptIfRunning: Boolean) = false
            override fun isCancelled() = false
            override fun isDone() = true
            override fun get(): T = value
            override fun get(timeout: Long, unit: TimeUnit): T = value
        }
    }
}

@RestrictsSuspension
interface WorkableFutureBuilder {
    /**
     * The principal this future is running as.
     */
    val principal: Principal

    /**
     * Suspends this coroutine until the given future is present.
     *
     * If the given future is already done or cancelled, returns/throws
     * immediately without suspending the coroutine.
     *
     * @return the futures value
     * @throws Exception Forwarded from the [Future], including [CancellationException]
     * @throws PrincipalConflictException If the given future is a [WorkableFuture] and has a different [principal]
     *                                    than this one.
     */
    suspend fun <E> await(future: Future<E>): E

    /**
     * Same as [await(Future)] but calls [finally] first. This is effectively syntax sugar
     * so that the finally code appears in the order it actually runs.
     *
     * Turns
     *
     *     val future = startProcess()
     *     finally { cleanUp(); }
     *     val result = await(future)
     *
     * into
     *
     *     val result = awaitAndFinally(startProcess()) { cleanUp() }
     */
    suspend fun <E> awaitAndFinally(future: Future<E>, finally: () -> Any?): E {
        finally(finally)
        return await(future)
    }

    /**
     * The given code will run before this future completes (Also when cancelled).
     *
     * When the this function is called multiple times the different functions will
     * be executed in reverse order.
     */
    fun finally(code: () -> Any?)

    /**
     * Logically like [LazySequence.foldRemaining] with these differences in
     * technical behaviour:
     * * if this future is cancelled, the sequence is closed.
     * * calls to [WorkableFuture.step] are deferred to the sequence
     * @throws PrincipalConflictException If the given sequence belongs to another principal.
     */
    suspend fun <Element : Any, Carry> foldRemaining(sequence: LazySequence<Element>, initial: Carry, accumulator: (Carry, Element) -> Carry): Carry
}

fun <T> launchWorkableFuture(principal: Any, code: suspend WorkableFutureBuilder.() -> T): WorkableFuture<T> = WorkableFutureImpl(principal, code)

class WorkableFutureNotCancellableException(message: String? = null, cause: Throwable? = null) : RuntimeException(message, cause)

fun <T, R> WorkableFuture<T>.map(mapper: (T) -> R): WorkableFuture<R> = object : WorkableFuture<R> {
    private val mapped = AtomicReference<R>(null)

    override val principal = this@map.principal
    override fun step() = this@map.step()
    override fun cancel(mayInterruptIfRunning: Boolean): Boolean = this@map.cancel(mayInterruptIfRunning)
    override fun isCancelled() = this@map.isCancelled
    override fun isDone() = this@map.isDone
    override fun get(): R {
        mapped.get()?.let { return it }
        val mappedValue = mapper(this@map.get())
        return mapped.compareAndExchange(null, mappedValue) ?: mappedValue
    }

    override fun get(timeout: Long, unit: TimeUnit): R {
        mapped.get()?.let { return it }
        val mappedValue = mapper(this@map.get(timeout, unit))
        return mapped.compareAndExchange(null, mappedValue) ?: mappedValue
    }
}