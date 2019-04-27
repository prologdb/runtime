package com.github.prologdb.runtime.util

/**
 * Assumes an immutable base [List] and provides low-overhead sub-lists
 * of that list.
 *
 * A prolog string is the same as a list of all the unicode code points of its
 * characters (in order and with duplicates). Therefore, a prolog string needs
 * an interface to prolog lists. This class takes advantage of immutability
 * of prolog terms and avoids unnecessary data copying.
 */
class ImmutableSubList<T>(
    private val source: List<T>,
    private val startOffset: Int,
    override val size: Int
) : List<T> {

    init {
        if (startOffset < 0) throw IndexOutOfBoundsException("startOffset must be 0 or positive")
        if (size < 0) throw IndexOutOfBoundsException("size must be 0 or positive")

        if (startOffset + size > source.size) {
            throw IndexOutOfBoundsException("${startOffset + size}")
        }
    }

    override fun contains(element: T): Boolean {
        element!!

        for (i in 0 until size) {
            val el = source[startOffset + i]
            if (el == element) return true
        }

        return false
    }
    override fun containsAll(elements: Collection<T>): Boolean = elements.all(this::contains)

    override fun get(index: Int): T {
        if (index >= size) throw IndexOutOfBoundsException("$index")
        return source[startOffset + index]
    }

    override fun indexOf(element: T): Int {
        element!!

        if (size == 1) return if (source[startOffset] == element) 0 else -1

        for (i in 0 until size) {
            val el = source[startOffset + i]
            if (el == element) return i
        }

        return -1
    }

    override fun isEmpty(): Boolean = size == 0

    override fun iterator(): Iterator<T> = listIterator()

    override fun lastIndexOf(element: T): Int {
        element!!

        if (size == 1) return if (source[startOffset] == element) 0 else -1

        for (i in lastIndex downTo 0) {
            val el = source[startOffset + i]
            if (el == element) return i
        }

        return -1
    }

    override fun listIterator(): ListIterator<T> = listIterator(0)

    override fun listIterator(index: Int): ListIterator<T> {
        if (index < 0 || index >= size) throw IndexOutOfBoundsException("$index")
        return IndexBasedSubIteratorOverImmutableList(source, startOffset + index, size - index)
    }

    override fun subList(fromIndex: Int, toIndex: Int): ImmutableSubList<T> {
        if (fromIndex < 0 || fromIndex > size) throw IndexOutOfBoundsException("$fromIndex")
        if (toIndex < 0 || toIndex > size) throw IndexOutOfBoundsException("$toIndex")
        if (toIndex < fromIndex) throw IllegalArgumentException()

        if (fromIndex == 0 && toIndex == lastIndex) return this

        return ImmutableSubList(source, startOffset + fromIndex, toIndex - fromIndex)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is List<*>) return false
        if (other.size != this.size) return false

        // compare element by element; driven by the other lists
        // iterator because the other list might not be RandomAccess (e.g. LinkedList)
        for ((selfIndex, otherEl) in other.withIndex()) {
            val selfEl = source[startOffset + selfIndex]
            if (selfEl != otherEl) return false
        }

        return true
    }

    override fun hashCode(): Int {
        var result = 0
        for (el in this) {
            result = 31 * result + el!!.hashCode()
        }

        return result
    }

    override fun toString(): String =  this.joinToString(", ", "[", "]")
}

private class IndexBasedSubIteratorOverImmutableList<out T>(
    private val list: List<T>,
    private val initialIndex: Int = 0,
    private val nElements: Int = list.size
) : ListIterator<T> {

    init {
        if (initialIndex < 0) throw IndexOutOfBoundsException("initialIndex must be 0 or positive")
        if (initialIndex + nElements > list.size) throw IndexOutOfBoundsException()
    }

    private var nextIndex: Int = initialIndex

    override fun hasNext(): Boolean = nextIndex - initialIndex < nElements
    override fun next(): T {
        if (!hasNext()) throw NoSuchElementException()
        return list[nextIndex++]
    }

    override fun hasPrevious(): Boolean = nextIndex > initialIndex

    override fun nextIndex(): Int = if (hasNext()) nextIndex else nElements

    override fun previous(): T {
        if (nextIndex <= initialIndex) throw NoSuchElementException()
        return list[--nextIndex]
    }

    override fun previousIndex(): Int = if (hasPrevious()) nextIndex - 1 else -1
}