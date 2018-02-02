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
     * @return The next element that matches the given predicate
     */
    fun find(predicate: (T) -> Boolean): T? {
        var baseResult: T
        var predicateResult: Boolean
        do {
            baseResult = tryAdvance() ?: return null
            predicateResult = predicate(baseResult)
        } while (!predicateResult)

        return if (predicateResult) baseResult else null
    }

    /**
     * Invokes the given action for all remaining elements in this [LazySequence]
     */
    fun forEachRemaining(consumer: (T) -> Unit) {
        while (true) consumer(tryAdvance() ?: break)
    }

    /**
     * Adds all elements remaining in this [LazySequence] to the collection obtained by invoking the given [supplier].
     *
     * E.g. use like this: `remainingTo(::LinkedList)`
     *
     * @return The collection obtained from [supplier]
     */
    fun <C : MutableCollection<in T>> remainingTo(supplier: () -> C): C {
        val target = supplier()
        forEachRemaining { target.add(it) }
        return target
    }

    /**
     * Returns a [LazySequence] of only those elements remaining in this [LazySequence] that match the given predicate.
     *
     * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
     */
    fun filterRemaining(predicate: (T) -> Boolean): LazySequence<T>
        = FilteredLazySequence(this, predicate)

    /**
     * Returns a [LazySequence] where each element is the result of invoking the given [mapper] with an element from
     * this [LazySequence]; order is preserved.
     *
     * *Consuming elements from the returned [LazySequence] also consumes them from this [LazySequence]*
     */
    fun <M> mapRemaining(mapper: (T) -> M): LazySequence<M>
        = MappedLazySequence(this, mapper)

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
     *
     * @see Collection.drop
     */
    fun skip(n: Int) {
        for (i in 1..n) tryAdvance()
    }
}