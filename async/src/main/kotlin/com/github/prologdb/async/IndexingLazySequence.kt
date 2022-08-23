package com.github.prologdb.async

class IndexingLazySequence<T : Any>(private val base: LazySequence<T>) : LazySequence<Pair<Long, T>> {
    @Volatile
    private var counter: Long = 0
    override val principal = base.principal
    override fun step() = base.step()
    override val state: LazySequence.State get() = base.state
    override fun tryAdvance(): Pair<Long, T>? {
        return base.tryAdvance()?.let { Pair(counter++, it) }
    }

    override fun close() {
        base.close()
    }
}