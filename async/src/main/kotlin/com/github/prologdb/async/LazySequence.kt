package com.github.prologdb.async

import java.util.concurrent.Future

/**
 * A lazysequence that calculates / obtains the next element only when it is actually needed (as opposed to when
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
interface LazySequence<T> {
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
        = DistinctLazySequence(this, selector)

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
    fun skip(n: Int): Int {
        var skipped = 0
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
    fun consumeAll() {
        while (true) tryAdvance() ?: break
    }

    companion object {
        fun <T> of(vararg elements: T): LazySequence<T> = of(elements, IrrelevantPrincipal)
            
        fun <T> of(elements: Array<out T>, principal: Principal): LazySequence<T> {
            if (elements.isEmpty()) return empty(principal)
            if (elements.size == 1) return singleton(elements[0], principal)

            return object : LazySequence<T> {
                override val principal = principal
                private var index = 0
                private var closed = false

                override fun step(): State = state

                override val state: State
                    get() = if (closed || index >= elements.size) State.DEPLETED else State.RESULTS_AVAILABLE

                override fun tryAdvance(): T? {
                    if (closed || index >= elements.size) return null
                    return elements[index++]
                }

                override fun close() {
                    closed = true
                }
            }
        }

        /**
         * @param element must not be null
         * @return a sequence of only the given element
         */
        fun <T> singleton(element: T, principal: Principal = IrrelevantPrincipal): LazySequence<T> {
            if (element == null) throw IllegalArgumentException("The given element must not be null.")

            return object : LazySequence<T> {
                override val principal = principal
                private var consumed = false

                override val state: State
                    get() = if (consumed) State.DEPLETED else State.RESULTS_AVAILABLE

                override fun step() = state

                override fun tryAdvance(): T? = if (consumed) null else { consumed = true; element }

                override fun close() {
                    consumed = true
                }
            }
        }

        /**
         * Much like Javas Optional.ofNullable: if the given element is not null,
         * the result of this function equals `LazySequence.singleton(thing)`. If it is null,
         * the result of this function equals `LazySequence.empty()`.
         */
        fun <T> ofNullable(thing: T?, principal: Principal = IrrelevantPrincipal): LazySequence<T> = if (thing == null) empty(principal) else singleton(thing, principal)

        /**
         * Creates an infinite [LazySequence] that obtains its values from the given [generator]. Ends
         * once [close] is called.
         *
         * @param generator Must not modify shared state or read from mutable shared state (in other words: be pure).
         */
        fun <T> fromGenerator(generator: () -> T?): LazySequence<T> {
            return fromGenerator(IrrelevantPrincipal, generator)
        }

        /**
         * Creates an infinite [LazySequence] that obtains its values from the given [generator]. Ends
         * once [close] is called.
         *
         * @param principal The principal the result sequence should belong to.
         * @param generator Must not modify shared state or read from mutable shared state (in other words: be pure).
         */
        fun <T> fromGenerator(principal: Principal, generator: () -> T?): LazySequence<T> {
            return object : LazySequence<T> {
                override val principal = principal
                var closed = false
                var cached: T? = null

                override fun step(): State {
                    if (closed) return State.DEPLETED
                    if (cached == null) {
                        cached = generator()
                        if (cached == null) {
                            closed = true
                            return State.DEPLETED
                        }
                    }

                    return State.RESULTS_AVAILABLE
                }

                override val state: State
                    get() = if (closed) State.DEPLETED else if (cached == null) State.PENDING else State.RESULTS_AVAILABLE

                override fun tryAdvance(): T? {
                    if (closed) return null

                    when (step()) {
                        State.RESULTS_AVAILABLE -> {
                            val result = cached
                            cached = null
                            return result
                        }
                        State.DEPLETED -> return null
                        else -> throw IllegalStateException()
                    }
                }

                override fun close() {
                    closed = true
                }
            }
        }

        private val emptySequence = object : LazySequence<Any> {
            override val principal = IrrelevantPrincipal
            override val state = State.DEPLETED
            override fun step() = State.DEPLETED
            override fun tryAdvance(): Any? = null
            override fun close() = Unit
        }

        fun <T> empty(principal: Principal): LazySequence<T> {
            if (principal == IrrelevantPrincipal) return empty()
            return object : LazySequence<T> {
                override val principal = principal
                override val state = State.DEPLETED
                override fun step() = State.DEPLETED
                override fun tryAdvance(): T? = null
                override fun close() = Unit
            }
        }
        
        fun <T> empty(): LazySequence<T> {
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

interface LazySequenceBuilder<T> {
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
     * Yields the given object as one result to the lazy sequence
     */
    suspend fun yield(result: T)

    /**
     * Yields all results of the given collection from this lazy sequence.
     */
    suspend fun yieldAll(results: Iterable<T>) {
        for (result in results) {
            yield(result)
        }
    }

    /**
     * Yields all results of the given lazy sequence to this lazy sequence.
     * @throws PrincipalConflictException If the given sequence is of another principal.
     */
    suspend fun yieldAll(results: LazySequence<T>)
}

fun <T> buildLazySequence(principal: Any, code: suspend LazySequenceBuilder<T>.() -> Unit): LazySequence<T> = LazySequenceImpl(principal, code)

/**
 * Adds all elements remaining in this [LazySequence] to the collection obtained by invoking the given [supplier].
 *
 * E.g. use like this: `remainingTo(::LinkedList)`
 *
 * @return The collection obtained from [supplier]
 */
fun <T, C : MutableCollection<in T>> LazySequence<T>.remainingTo(supplier: () -> C): C {
    val target = supplier()
    forEachRemaining { target.add(it) }
    return target
}

/**
 * @return The next element that matches the given predicate
 */
fun <T> LazySequence<T>.find(predicate: (T) -> Boolean): T? {
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
fun <T> LazySequence<T>.filterRemaining(predicate: (T) -> Boolean): LazySequence<T>
    = FilteredLazySequence(this, predicate)

/**
 * Returns a [LazySequence] where each element is the result of invoking the given [mapper] with an element from
 * this [LazySequence]; order is preserved.
 *
 * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
 */
fun <T, M> LazySequence<T>.mapRemaining(mapper: (T) -> M): LazySequence<M>
    = MappedLazySequence(this, mapper)

/**
 * For every remaining element in `this`, invokes the given mapper, drains the resulting sequence yielding
 * every element.
 *
 * *[LazySequence.step]ing and [LazySequence.tryAdvance]ing this sequence may consume elements from this [LazySequence]*
 */
fun <T, M> LazySequence<T>.flatMapRemaining(mapper: suspend LazySequenceBuilder<M>.(T) -> Unit): LazySequence<M>
    = FlatMapLazySequence(this, mapper)

/**
 * Returns a [LazySequence] where exceptions thrown on each call to [LazySequence.tryAdvance]
 * are first put through the given mapper before being rethrown.
 *
 * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
 */
inline fun <T, reified E> LazySequence<T>.transformExceptionsOnRemaining(noinline mapper: (E) -> Throwable): LazySequence<T>
    where E : Throwable {
    if (E::class == Throwable::class) {
        return RethrowingExceptionMappingLazySequence(this, mapper as (Throwable) -> Throwable)
    } else {
        return RethrowingExceptionMappingLazySequence(this, { e: Throwable ->
            if (e is E) mapper(e) else e
        })
    }
}

/**
 * Invokes the given action for all remaining elements in this [LazySequence]
 */
inline fun <T> LazySequence<T>.forEachRemaining(consumer: (T) -> Unit) {
    while (true) consumer(tryAdvance() ?: break)
    close()
}

/**
 * Aggregates the remaining elements of the sequence starting with the given initial
 * value and, for each element, applying the given accumulator.
 * @return the last return value of the accumulator
 */
inline fun <T, R> LazySequence<T>.foldRemaining(initial: R, accumulator: (T, R) -> R): R {
    var carry = initial
    while (true) {
        val el = tryAdvance() ?: break
        carry = accumulator(el, carry)
    }

    return carry
}

/**
 * Adds the remaining elements to a list and returns that list.
 */
fun <T> LazySequence<T>.remainingToList(): List<T> {
    val target = ArrayList<T>()
    forEachRemaining { target.add(it) }
    return target
}