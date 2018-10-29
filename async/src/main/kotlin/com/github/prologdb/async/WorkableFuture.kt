package com.github.prologdb.async

import java.util.concurrent.CancellationException
import java.util.concurrent.Future
import kotlin.coroutines.experimental.RestrictsSuspension

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
}

@RestrictsSuspension
interface WorkableFutureBuilder {
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
     */
    suspend fun <E> awaitAndFinally(future: Future<E>, finally: () -> Any?) {
        finally(finally)
        await(future)
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
     * * if the sequence is a [WorkableLazySequence]
     *   * calls to [WorkableFuture.step] are deferred to the sequence
     * @throws PrincipalConflictException If the given sequence belongs to another principal.
     */
    suspend fun <E, C> foldRemaining(sequence: LazySequence<E>, initial: C, accumulator: (C, E) -> C): C
}

fun <T> launchWorkableFuture(principal: Any, code: suspend WorkableFutureBuilder.() -> T): WorkableFuture<T> = WorkableFutureImpl(principal, code)

inline fun <R, F> awaitAndThen(principal: Any, future: Future<F>, crossinline code: suspend WorkableFutureBuilder.(F) -> R): WorkableFuture<R> = WorkableFutureImpl(principal) {
    code(await(future))
}

inline fun <R, F> awaitAndThen(future: WorkableFuture<F>, crossinline code: suspend WorkableFutureBuilder.(F) -> R): WorkableFuture<R> = WorkableFutureImpl(future.principal) {
    code(await(future))
}