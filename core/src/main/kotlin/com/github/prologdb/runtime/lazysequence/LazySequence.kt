package com.github.prologdb.runtime.lazysequence

/**
 * A lazysequence that calculates / obtains the next element only when it is actually needed (as opposed to when
 * the presence is queried, e.g. like [Iterator.hasNext]). This class is used over [Iterator] because [Iterator.hasNext]
 * implies a read-only action. But prolog solution sequences can have side-effects (e.g. with `retract/1`) and that
 * implication would be misleading.
 */
interface LazySequence<T> {
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
     * Accumulates value starting with [initial] value and applying [accumulator] for each remaining element in this
     * sequence.
     *
     * @see Sequence.fold
     */
    fun <C> foldRemaining(initial: C, accumulator: (C, T) -> C): C {
        var carry: C = initial
        forEachRemaining {
            carry = accumulator(carry, it)
        }

        return carry
    }

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
     * Consumes all elements of this sequence. Useful for sequences where consuming has side effects and the user
     * only care about the side effects (instead of the elements).
     */
    fun consumeAll() {
        while (true) tryAdvance() ?: break
    }

    companion object {
        fun <T> of(vararg elements: T): LazySequence<T> {
            if (elements.isEmpty()) return empty()
            if (elements.size == 1) return singleton(elements[0])

            return object : LazySequence<T> {
                private var index = 0
                private var closed = false
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
        fun <T> singleton(element: T): LazySequence<T> {
            if (element == null) throw IllegalArgumentException("The given element must not be null.")

            return object : LazySequence<T> {
                private var consumed = false
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
        fun <T> ofNullable(thing: T?): LazySequence<T> = if (thing == null) empty() else singleton(thing)

        fun <T> fromGenerator(generator: () -> T?): LazySequence<T> {
            return object : LazySequence<T> {
                var closed = false

                override fun tryAdvance(): T? {
                    if (closed) return null

                    val result = generator()
                    if (result == null) closed = true
                    return result
                }

                override fun close() {
                    closed = true
                }
            }
        }

        private val emptySequence = object : LazySequence<Any> {
            override fun tryAdvance(): Any? = null
            override fun close() = Unit
        }

        fun <T> empty(): LazySequence<T> {
            @Suppress("unchecked_cast") // no elements returned from the sequence; the size does not matter
            return emptySequence as LazySequence<T>
        }
    }
}

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
 * @return a [LazySequence] of all the elements remaining in this [LazySequence] that are not null.
 */
fun <T> LazySequence<T?>.filterRemainingNotNull(): LazySequence<T>
    = @Suppress("unchecked") FilteredLazySequence(this, { it != null}) as LazySequence<T>

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
 * Invokes the given action for all remaining elements in this [LazySequence]
 */
inline fun <T> LazySequence<T>.forEachRemaining(consumer: (T) -> Unit) {
    while (true) consumer(tryAdvance() ?: break)
    close()
}