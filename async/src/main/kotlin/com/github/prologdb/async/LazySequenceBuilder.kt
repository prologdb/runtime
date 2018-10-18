package com.github.prologdb.async

import java.util.concurrent.Future

interface LazySequenceBuilder<T> {
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
     * Yields all results of the given collection from this lazy sequence.
     */
    suspend fun yieldAll(results: Collection<T>)

    /**
     * Yields all results of the given lazy sequence to this lazy sequence.
     */
    suspend fun yieldAll(results: LazySequence<T>)
}

fun <T> buildLazySequence(code: suspend LazySequenceBuilder<T>.() -> Unit): LazySequence<T> = LazySequenceImpl(code)