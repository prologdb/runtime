package com.github.tmarsteel.ktprolog.parser.sequence

class IteratorBasedTransactionalSequence<T>(private val iterator: Iterator<T>) : TransactionalSequence<T, Index> {

    private val buffer: MutableList<T> = ArrayList(100)

    /**
     * Markers as indexes into the buffer
     */
    private val markers: MutableList<Int> = ArrayList(20)

    /**
     * The current position within the buffer; is not meaningful when the buffer is empty
     */
    private var currentPosition: Int = 0

    override fun hasNext(): Boolean {
        if (buffer.isEmpty() || currentPosition >= buffer.size) {
            return iterator.hasNext()
        }
        else {
            return true
        }
    }

    override fun next(): T {
        if (!hasNext()) throw IllegalStateException()

        if (buffer.isEmpty() || currentPosition >= buffer.size) {
            val next = iterator.next()

            if (markers.isNotEmpty()) {
                buffer.add(next)
                currentPosition++
            }

            return next
        }
        else {
            val next = buffer[currentPosition]
            currentPosition++

            return next
        }
    }

    override fun mark() {
        if (markers.isEmpty()) {
            markers.add(0)
        }
        else
        {
            markers.add(currentPosition)
        }
    }

    override fun commit() {
        if (markers.isEmpty()) {
            throw IllegalStateException("No marker to commit")
        }

        markers.removeAt(markers.size - 1)
    }

    override fun rollback() {
        if (markers.isEmpty()) {
            throw IllegalStateException("No marker to roll back to")
        }

        currentPosition = markers[markers.size - 1]
        markers.removeAt(markers.size - 1)
    }
}