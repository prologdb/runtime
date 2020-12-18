package com.github.prologdb.async

class RethrowingExceptionMappingLazySequence<T : Any>(
    private val nested: LazySequence<T>,
    private val mapper: (Throwable) -> Throwable
) : LazySequence<T> {
    override val principal = nested.principal

    override fun step() = nested.step()

    override val state: LazySequence.State
        get() = nested.state

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