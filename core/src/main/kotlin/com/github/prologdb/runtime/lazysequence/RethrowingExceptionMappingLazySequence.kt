package com.github.prologdb.runtime.lazysequence

class RethrowingExceptionMappingLazySequence<T>(
    private val nested: LazySequence<T>,
    private val mapper: (Throwable) -> Throwable
) : LazySequence<T> {
    override fun tryAdvance(): T? {
        try {
            return nested.tryAdvance()
        } catch (ex: Throwable) {
            throw mapper(ex)
        }
    }

    override fun close() {
        nested.close()
    }
}