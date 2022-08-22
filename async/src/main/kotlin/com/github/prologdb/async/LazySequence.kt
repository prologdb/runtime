package com.github.prologdb.async

import java.util.concurrent.Future

/**
 * A sequence that calculates / obtains the next element only when it is actually needed (as opposed to when
 * the presence is queried, e.g. like [Iterator.hasNext]). This class is used over [Iterator] because [Iterator.hasNext]
 * implies a read-only action. But prolog solution sequences can have side-effects (e.g. with `retract/1`) and that
 * implication would be misleading.
 *
 * The calculation of results can be done step by step where the steps, ideally, are at boundaries between
 * CPU/memory bound tasks and I/O bound tasks. This aims to optimize resource utilization in a situation where
 * there are large numbers of [LazySequence]s open at the same time.
 *
 * Calling [tryAdvance] will perform **all** necessary steps to calculate the next result (or determine
 * that there are no more). For more fine grained control and information use [step] and [state].
 */
interface LazySequence<T : Any> {
    /**
     * The concurrency principal. Instead of [Thread], this is used to obtain locks and mutexes in the
     * name of the coroutine. Traditional [synchronized] blocks are still used to prevent multiple threads
     * from running the same coroutine simultaneously.
     */
    val principal: Principal

    /**
     * Performs CPU&memory bound work for the next element in the
     * sequence and returns when waiting for I/O bound tasks
     * (disk, memory, ...).
     *
     * When this method is called in state [State.RESULTS_AVAILABLE],
     * it is not defined whether calculations are done.
     *
     * This function can only be run by one thread at a time.
     * Implementations are inclined to use [synchronized] to achieve
     * this behaviour, [java.util.concurrent.locks.Lock]s are another
     * option, though.
     *
     * @return the state of this sequence after this call
     */
    fun step(): State

    val state: State

    /**
     * Attempts to compute or obtain the next element.
     * @return the next element in the [LazySequence] or `null` when there are no more elements.
     */
    fun tryAdvance(): T?

    /**
     * After a call to this method, [tryAdvance] will always return `null`. Releases as much of the resources
     * associated with this sequence as possible.
     *
     * Repeated invocations to this method **must not throw**.
     */
    fun close()

    /**
     * Returns a [LazySequence] that, from the remaining elements in this sequence, returns only distinct ones.
     *
     * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
     *
     * @see Sequence.distinct
     */
    fun distinct(): LazySequence<T> = distinctBy({ it })

    /**
     * Returns a [LazySequence] that, from the remaining elements in this sequence, returns only those that have
     * a distinct key as returned by [selector].
     *
     * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
     *
     * @see Sequence.distinctBy
     */
    fun <K> distinctBy(selector: (T) -> K): LazySequence<T>
        = MapNotNullAndFilterLazySequence(this, { it }, distinctFilter(selector))

    /**
     * Consumes all of the remaining elements in this sequence.
     *
     * @return the least element, according to [comparator] or `null` if there are no elements in this sequence
     */
    fun minWith(comparator: Comparator<T>): T? {
        var pivot: T = tryAdvance() ?: return null

        forEachRemaining {
            if (comparator.compare(it, pivot) < 0) {
                pivot = it
            }
        }

        return pivot
    }

    /**
     * Consumes all of the remaining elements in this sequence.
     *
     * @return the element with the least key as selected by [selector] or `null` if there are no elements in this sequence
     */
    fun <K : Comparable<K>> minBy(selector: (T) -> K): T? {
        var pivot: T = tryAdvance() ?: return null
        var pivotKey = selector(pivot)

        forEachRemaining { el ->
            val elKey = selector(el)
            if (elKey < pivotKey) {
                pivot = el
                pivotKey = elKey
            }
        }

        return pivot
    }

    /**
     * Consumes all of the remaining elements in this sequence.
     *
     * @return the element with the least key as selected by [selector] according to [comparator]
     *         or `null` if there are no elements in this sequence
     */
    fun <K> minByWith(selector: (T) -> K, comparator: Comparator<K>): T? {
        var pivot: T = tryAdvance() ?: return null
        var pivotKey = selector(pivot)

        forEachRemaining { el ->
            val elKey = selector(el)
            if (comparator.compare(elKey, pivotKey) < 0) {
                pivot = el
                pivotKey = elKey
            }
        }

        return pivot
    }

    /**
     * Consumes all of the remaining elements in this sequence.
     *
     * @return the greatest element, according to [comparator] or `null` if there are no elements in this sequence
     */
    fun maxWith(comparator: Comparator<T>): T? {
        var pivot: T = tryAdvance() ?: return null

        forEachRemaining {
            if (comparator.compare(it, pivot) > 0) {
                pivot = it
            }
        }

        return pivot
    }

    /**
     * Consumes all of the remaining elements in this sequence.
     *
     * @return the element with the greatest key as selected by [selector] or `null` if there are no elements in this sequence
     */
    fun <K : Comparable<K>> maxBy(selector: (T) -> K): T? {
        var pivot: T = tryAdvance() ?: return null
        var pivotKey = selector(pivot)

        forEachRemaining { el ->
            val elKey = selector(el)
            if (elKey < pivotKey) {
                pivot = el
                pivotKey = elKey
            }
        }

        return pivot
    }

    /**
     * Consumes all of the remaining elements in this sequence.
     *
     * @return the element with the greatest key as selected by [selector] according to [comparator]
     *         or `null` if there are no elements in this sequence
     */
    fun <K> maxByWith(selector: (T) -> K, comparator: Comparator<K>): T? {
        var pivot: T = tryAdvance() ?: return null
        var pivotKey = selector(pivot)

        forEachRemaining { el ->
            val elKey = selector(el)
            if (comparator.compare(elKey, pivotKey) > 0) {
                pivot = el
                pivotKey = elKey
            }
        }

        return pivot
    }

    /**
     * Skips/consumes the next [n] elements
     * @return The actual number of elements skipped. Is less than [n] if the sequence had fewer than [n] elements
     *         at the time of invocation.
     */
    fun skip(n: Long): Long {
        var skipped = 0L
        for (i in 1..n) {
            if (tryAdvance() == null) break
            skipped++
        }

        return skipped
    }

    /**
     * @return A sequence that, of the remaining elements, emits at most [n]. As soon as [n]
     *         elements have been emitted, both this sequence and the returned sequence are closed.
     */
    fun limitRemaining(n: Long): LazySequence<T> {
        return LimitingLazySequence(this, n)
    }

    /**
     * Consumes all elements of this sequence. Useful for sequences where consuming has side effects and the user
     * only cares about the side effects (instead of the elements).
     */
    fun consumeAll(): WorkableFuture<Unit> {
        return launchWorkableFuture(principal) {
            this@launchWorkableFuture.foldRemaining(this@LazySequence, Unit) { _, _ -> }
        }
    }

    companion object {
        fun <T : Any> ofIterable(elements: Iterable<T>, principal: Principal = IrrelevantPrincipal): LazySequence<T> = when {
            elements is List<T> && elements is RandomAccess -> RandomAccessListLazySequence(elements, principal)
            else -> IteratorLazySequence(elements, principal)
        }

        fun <T : Any> of(vararg elements: T): LazySequence<T> = of(elements, IrrelevantPrincipal)
            
        fun <T : Any> of(elements: Array<out T>, principal: Principal): LazySequence<T> {
            if (elements.isEmpty()) return empty(principal)
            if (elements.size == 1) return singleton(elements[0], principal)

            return ArrayLazySequence(elements, principal)
        }

        /**
         * @param element must not be null
         * @return a sequence of only the given element
         */
        fun <T : Any> singleton(element: T, principal: Principal = IrrelevantPrincipal): LazySequence<T> = SingletonLazySequence(element, principal)

        /**
         * Much like Javas Optional.ofNullable: if the given element is not null,
         * the result of this function equals `LazySequence.singleton(thing)`. If it is null,
         * the result of this function equals `LazySequence.empty()`.
         */
        fun <T : Any> ofNullable(thing: T?, principal: Principal = IrrelevantPrincipal): LazySequence<T> = if (thing == null) empty(principal) else singleton(thing, principal)

        /**
         * Creates an infinite [LazySequence] that obtains its values from the given [generator]. Ends
         * once [close] is called.
         *
         * @param generator Must not modify shared state or read from mutable shared state (in other words: be pure).
         */
        fun <T : Any> fromGenerator(generator: () -> T?): LazySequence<T> {
            return fromGenerator(IrrelevantPrincipal, generator)
        }

        /**
         * Creates an infinite [LazySequence] that obtains its values from the given [generator]. Ends
         * once [close] is called.
         *
         * @param principal The principal the result sequence should belong to.
         * @param generator Must not modify shared state or read from mutable shared state (in other words: be pure).
         */
        fun <T : Any> fromGenerator(principal: Principal, generator: () -> T?): LazySequence<T> = GeneratorLazySequence(principal, generator)

        private val emptySequence = object : LazySequence<Any> {
            override val principal = IrrelevantPrincipal
            override val state = State.DEPLETED
            override fun step() = State.DEPLETED
            override fun tryAdvance(): Any? = null
            override fun close() = Unit
        }

        fun <T : Any> empty(principal: Principal): LazySequence<T> {
            if (principal == IrrelevantPrincipal) return empty()
            return object : LazySequence<T> {
                override val principal = principal
                override val state = State.DEPLETED
                override fun step() = State.DEPLETED
                override fun tryAdvance(): T? = null
                override fun close() = Unit
            }
        }
        
        fun <T : Any> empty(): LazySequence<T> {
            @Suppress("unchecked_cast") // no elements returned from the sequence; the type does not matter
            return emptySequence as LazySequence<T>
        }
    }

    enum class State {
        /**
         * No results are immediately available. Either applies:
         * * the next step is also CPU&memory bound and calling [step] again would
         *   advance the process of calculating the solution
         * * the next step is I/O bound and is waiting for that to finish. In this case,
         *   calling [step] again does not have any effect.
         */
        PENDING,

        /**
         * Results are available for immediate grab without calculation
         * from [tryAdvance]. After grabbing all results the sequence will
         * transition to any of the other states.
         */
        RESULTS_AVAILABLE,

        /**
         * All results have been calculated and obtained via [tryAdvance]
         */
        DEPLETED,

        /**
         * An error occured while calculating the next result.
         * [tryAdvance] will throw that error.
         */
        FAILED
    }
}

interface LazySequenceBuilder<T : Any> {
    /** The principal of the sequence being built. To be used to initialize sub-sequences. */
    val principal: Principal

    /**
     * Suspends this coroutine until the given future is present.
     *
     * If the given future is already done or cancelled, returns/throws
     * immediately without suspending the coroutine.
     *
     * @return the futures value
     * @throws Exception Forwarded from the [Future], including [java.util.concurrent.CancellationException]
     * @throws PrincipalConflictException If the given future is a [WorkableFuture] and has a different [principal]
     *                                    than this one.
     */
    suspend fun <E> await(future: Future<E>): E

    /**
     * **DO NOT USE AS THE LAST INVOCATION IN A BUILDER; return the value instead.**
     *
     * Yields the given object as one result to the lazy sequence
     */
    suspend fun yield(result: T)

    /**
     * **DO NOT USE AS THE LAST INVOCATION IN A BUILDER; use [yieldAllFinal] or [flatMapRemaining] instead.**
     *
     * Yields all results of the given collection from this lazy sequence.
     */
    suspend fun yieldAll(results: Iterable<T>) {
        for (result in results) {
            yield(result)
        }
    }

    /**
     * Yields all but the last element from the given [Iterable]. The last element
     * is returned from this function. This is supposed to be used **exclusively** in this fashion:
     *
     *     return yieldAllFinal(results)
     */
    suspend fun yieldAllFinal(results: Sequence<T>): T? {
        val iterator = results.iterator()
        var element: T? = null
        while (iterator.hasNext()) {
            element = iterator.next()
            if (!iterator.hasNext()) {
                break
            }

            yield(element)
        }

        return element
    }

    /**
     * Yields all results of the given lazy sequence to this lazy sequence.
     *
     * **DO NOT USE AS THE LAST INVOCATION IN A BUILDER; use [yieldAllFinal] or [flatMapRemaining] instead.**
     * @throws PrincipalConflictException If the given sequence is of another principal.
     */
    suspend fun yieldAll(results: LazySequence<T>)

    /**
     * Yields all but the last element from the given [LazySequence]. The last element
     * is returned from this function. This is supposed to be used **exclusively** in this fashion:
     *
     *     return yieldAllFinal(results)
     *
     * @throws PrincipalConflictException If the given sequence is of another principal.
     */
    suspend fun yieldAllFinal(results: LazySequence<T>): T?
}

fun <T : Any> buildLazySequence(principal: Any, code: suspend LazySequenceBuilder<T>.() -> T?): LazySequence<T> = LazySequenceImpl(principal, code)

/**
 * Adds all elements remaining in this [LazySequence] to the collection obtained by invoking the given [supplier].
 *
 * E.g. use like this: `remainingTo(::LinkedList)`
 *
 * @return The collection obtained from [supplier]
 */
fun <T : Any, C : MutableCollection<in T>> LazySequence<T>.remainingTo(supplier: () -> C): WorkableFuture<C> {
    val target = supplier()
    return forEachRemaining { target.add(it) }.map { target }
}

/**
 * @return The next element that matches the given predicate
 */
fun <T : Any> LazySequence<T>.find(predicate: (T) -> Boolean): T? {
    var baseResult: T
    var predicateResult: Boolean
    do {
        baseResult = tryAdvance() ?: return null
        predicateResult = predicate(baseResult)
    } while (!predicateResult)

    return if (predicateResult) baseResult else null
}

/**
 * Returns a [LazySequence] of only those elements remaining in this [LazySequence] that match the given predicate.
 *
 * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
 */
fun <T : Any> LazySequence<T>.filterRemaining(predicate: (T) -> Boolean): LazySequence<T>
    = MapNotNullAndFilterLazySequence(this, { it }, predicate)

/**
 * Returns a [LazySequence] where each element is the result of invoking the given [mapper] with an element from
 * this [LazySequence]; order is preserved.
 *
 * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
 */
fun <T : Any, M : Any> LazySequence<T>.mapRemaining(mapper: (T) -> M): LazySequence<M>
    = MappedLazySequence(this, mapper)

fun <T : Any, M : Any> LazySequence<T>.mapRemainingNotNull(mapper: (T) -> M?): LazySequence<M>
    = MapNotNullAndFilterLazySequence(this, mapper, { true })

/**
 * For every remaining element in `this`, invokes the given mapper, drains the resulting sequence yielding
 * every element.
 *
 * *[LazySequence.step]ing and [LazySequence.tryAdvance]ing this sequence may consume elements from this [LazySequence]*
 */
fun <T : Any, M : Any> LazySequence<T>.flatMapRemaining(mapper: suspend LazySequenceBuilder<M>.(T) -> M?): LazySequence<M>
    = FlatMapLazySequence(this, mapper)

/**
 * Returns a [LazySequence] where exceptions thrown on each call to [LazySequence.tryAdvance]
 * are first put through the given mapper before being rethrown.
 *
 * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
 */
inline fun <T : Any, reified E> LazySequence<T>.transformExceptionsOnRemaining(noinline mapper: (E) -> Throwable): LazySequence<T> {
    return if (E::class == Throwable::class) {
        @Suppress("UNCHECKED_CAST")
        RethrowingExceptionMappingLazySequence(this, mapper as (Throwable) -> Throwable)
    } else {
        RethrowingExceptionMappingLazySequence(this) { e: Throwable ->
            if (e is E) mapper(e) else e
        }
    }
}

/**
 * Invokes the given action for all remaining elements in this [LazySequence]
 */
fun <T : Any> LazySequence<T>.forEachRemaining(consumer: (T) -> Unit): WorkableFuture<Unit> {
    return launchWorkableFuture(principal) {
        this@launchWorkableFuture.foldRemaining(this@forEachRemaining, Unit) { _, e -> consumer(e) }
    }
}

/**
 * Aggregates the remaining elements of the sequence starting with the given initial
 * value and, for each element, applying the given accumulator.
 * @return the last return value of the accumulator
 */
fun <T : Any, R> LazySequence<T>.foldRemaining(initial: R, accumulator: (R, T) -> R): WorkableFuture<R> {
    return launchWorkableFuture(principal) {
        this@launchWorkableFuture.foldRemaining(this@foldRemaining, initial, accumulator)
    }
}

/**
 * Adds the remaining elements to a list and returns that list.
 */
fun <T : Any> LazySequence<T>.remainingToList(): WorkableFuture<out List<T>> = remainingTo(::ArrayList)
