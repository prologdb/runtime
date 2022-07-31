package com.github.prologdb.async

class LimitingLazySequence<T : Any>(
    private val nested: LazySequence<T>,
    private val limit: Long
) : LazySequence<T> {
    init {
        require(limit >= 0)
    }

    var counter = 0L
    var closed = false
    var closedState: LazySequence.State = LazySequence.State.DEPLETED

    override val principal = nested.principal

    init {
        if (limit == 0L) {
            close()
        }
    }

    override fun step() = if (closed) closedState else nested.step()

    override val state
        get() = if (closed) closedState else nested.state

    override fun tryAdvance(): T? {
        val el = try {
            nested.tryAdvance()
        }
        catch (ex: Throwable) {
            closedState = nested.state
            closed = true
            throw ex
        }

        if (el == null) {
            closed = true
            closedState = LazySequence.State.DEPLETED
            return null
        }

        counter++
        if (counter >= limit) {
            closed = true
            closedState = LazySequence.State.DEPLETED
            nested.close()
        }

        return el
    }

    override fun close() {
        closed = true
        closedState = LazySequence.State.DEPLETED
        nested.close()
    }
}
