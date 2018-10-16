package com.github.prologdb.async

class MappedLazySequence<T, M>(
    private val base: LazySequence<T>,
    private val mapper: (T) -> M
) : LazySequence<M> {
    override fun tryAdvance(): M? {
        return mapper(base.tryAdvance() ?: return null)
    }

    override fun close() = base.close()
}