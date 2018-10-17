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
     */
    suspend fun <E> await(future: Future<E>): E
}

inline fun <T> launchWorkableFuture(noinline code: suspend WorkableFutureBuilder.() -> T): WorkableFuture<T> = WorkableFutureImpl(code)

inline fun <R, F> awaitAndThenWorkable(future: Future<F>, crossinline code: suspend WorkableFutureBuilder.(F) -> R): WorkableFuture<R> = WorkableFutureImpl {
    code(await(future))
}