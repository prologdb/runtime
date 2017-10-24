package com.github.tmarsteel.ktprolog.parser.sequence

interface TransactionalSequence<out ItemType, PositionType : TransactionalSequence.Companion.SequencePosition> : Iterator<ItemType> {
    /**
     * Marks the current position in the sequence. Marks stack: calling this method multiple times creates as many
     * markers. Each of them can be committed or rolled back in reverse order (e.g. `mark(); mark(); commit(); rollback()`).
     */
    fun mark()

    /**
     * Commits the most recent marker: removes that marker from the storage and keeps the position where it currently is.
     */
    fun commit()

    /**
     * Rolls the current position back to the most recent marker and removes that marker from the storage.
     */
    fun rollback()

    companion object {
        /**
         * Position within the transactional sequence
         */
        interface SequencePosition {
            val index: Int
        }
    }
}

class Index(override val index: Int) : TransactionalSequence.Companion.SequencePosition