package com.github.prologdb.parser.sequence

interface TransactionalSequence<out ItemType> : Iterator<ItemType> {
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
}