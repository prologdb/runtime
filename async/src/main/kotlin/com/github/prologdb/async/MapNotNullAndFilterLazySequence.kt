package com.github.prologdb.async

class MapNotNullAndFilterLazySequence<T : Any, M : Any>(
    private val base: LazySequence<T>,
    private val mapper: (T) -> M?,
    private val predicate: (M) -> Boolean
) : LazySequence<M> {
    override val principal = base.principal

    /** True when closed or errored */
    var closed = false

    /** If set, the base errored */
    var error: Throwable? = null

    /** Cached result from calculating in step() */
    var cached: M? = null

    override val state: LazySequence.State
        get() = when {
            error != null -> LazySequence.State.FAILED
            closed -> LazySequence.State.DEPLETED
            cached != null -> LazySequence.State.RESULTS_AVAILABLE
            else -> LazySequence.State.PENDING
        }

    override fun step(): LazySequence.State = step(false)

    private fun step(joinIfPending: Boolean): LazySequence.State {
        if (error != null) return LazySequence.State.FAILED
        if (closed) return LazySequence.State.DEPLETED
        if (cached != null) return LazySequence.State.RESULTS_AVAILABLE

        when (base.step()) {
            LazySequence.State.RESULTS_AVAILABLE -> {
                checkBaseResults@while (base.state == LazySequence.State.RESULTS_AVAILABLE) {
                    val baseValue = mapper(base.tryAdvance()!!)
                    if (baseValue != null && predicate(baseValue)) {
                        cached = baseValue
                        break@checkBaseResults
                    }
                }
            }
            LazySequence.State.DEPLETED -> {
                close()
            }
            LazySequence.State.FAILED -> {
                close()
                val throwable = try {
                    base.tryAdvance()
                    null
                } catch (ex: Throwable) {
                    ex
                } ?: throw IllegalStateException("Base sequence FAILED but tryAdvance() does not throw")

                error = throwable
            }
            LazySequence.State.PENDING -> {
                if (joinIfPending) {
                    val baseValue = base.tryAdvance()?.let(mapper)
                    if (baseValue == null) {
                        close()
                    }
                    else {
                        if (predicate(baseValue)) {
                            cached = baseValue
                        }
                        // else: was not distinct; cached remains null, still PENDING
                    }
                }
                // else: nothing to do
            }
        }

        return state
    }

    override tailrec fun tryAdvance(): M? {
        when {
            error != null -> throw error!!
            closed -> return null
            cached != null -> {
                val result = cached!!
                cached = null
                return result
            }
            else -> {
                step(joinIfPending = true)
                return tryAdvance()
            }
        }
    }

    override fun close() {
        closed = true
        base.close()
    }
}

fun <T, K> distinctFilter(selector: (T) -> K): (T) -> Boolean {
    val seenKeys: MutableSet<K> = HashSet()
    return {
        val key = selector(it)
        seenKeys.add(key) // if add returns false it was already there so not unique
    }
}