package com.github.prologdb.runtime.async

class FilteredLazySequence<T>(
    private val base: LazySequence<out T>,
    private val predicate: (T) -> Boolean
) : LazySequence<T> {
    override fun tryAdvance(): T? = base.find(predicate)
    override fun close() = base.close()
}