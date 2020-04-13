package com.github.prologdb.async

class MappedLazySequence<T : Any, M : Any>(
    private val base: LazySequence<T>,
    private val mapper: (T) -> M
) : LazySequence<M> {
    override val principal = base.principal

    override fun step() = base.step()

    override val state
        get() = base.state

    override fun tryAdvance() = base.tryAdvance()?.let(mapper)

    override fun close() = base.close()
}