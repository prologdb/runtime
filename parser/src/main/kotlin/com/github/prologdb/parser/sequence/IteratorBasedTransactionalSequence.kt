package com.github.prologdb.parser.sequence

open class IteratorBasedTransactionalSequence<T>(private val iterator: Iterator<T>) : TransactionalSequence<T> {

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
        if (!hasNext()) throw NoSuchElementException()

        if (buffer.isEmpty() || currentPosition >= buffer.size) {
            val next = iterator.next()

            if (markers.isEmpty()) {
                // reading past the buffer and there is no way that we need to roll back into the buffer again
                // => the buffer is no longer needed
                buffer.clear()
                currentPosition = 0
            }
            else
            {
                // there is a marker
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
        markers.add(currentPosition)
    }

    override fun commit() {
        if (markers.isEmpty()) {
            throw IllegalStateException("No marker to commit")
        }

        popMarker()
    }

    override fun rollback() {
        if (markers.isEmpty()) {
            throw IllegalStateException("No marker to roll back to")
        }

        currentPosition = markers[markers.size - 1]
        popMarker()
    }

    private fun popMarker() {
        markers.removeAt(markers.size - 1)

        if (markers.isEmpty() && currentPosition == buffer.size) {
            // buffer not needed anymore
            buffer.clear()
            currentPosition = 0
        }
    }
}