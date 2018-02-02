package com.github.prologdb.runtime.lazysequence

class MappedLazySequence<T, M>(
    private val base: LazySequence<T>,
    private val mapper: (T) -> M
) : LazySequence<M> {
    override fun tryAdvance(): M? {
        val el = base.tryAdvance() ?: return null
        return mapper(el)
    }
}