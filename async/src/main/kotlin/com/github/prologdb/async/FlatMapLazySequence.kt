package com.github.prologdb.async

import com.github.prologdb.async.LazySequence.State.DEPLETED
import com.github.prologdb.async.LazySequence.State.FAILED
import com.github.prologdb.async.LazySequence.State.PENDING
import com.github.prologdb.async.LazySequence.State.RESULTS_AVAILABLE

class FlatMapLazySequence<T : Any, M : Any>(
    private val nested: LazySequence<T>,
    private val mapper: suspend LazySequenceBuilder<M>.(T) -> M?
) : LazySequence<M> {
    override val principal = nested.principal

    private var inMapper = false
    private var currentMapperSequence: LazySequence<M>? = null

    override var state = PENDING
        private set

    private var error: Throwable? = null

    override tailrec fun step(): LazySequence.State {
        if (inMapper) {
            val subSequence = currentMapperSequence!!
            val subSequenceState = subSequence.step()
            state = subSequenceState

            when (subSequenceState) {
                RESULTS_AVAILABLE,
                PENDING -> {
                    return subSequenceState
                }
                DEPLETED -> {
                    inMapper = false
                    currentMapperSequence = null
                    state = PENDING
                    return step()
                }
                FAILED -> {
                    error = try {
                        subSequence.tryAdvance(); null
                    } catch (ex: Throwable) { ex }!!
                    return FAILED
                }
            }
        }
        else {
            val nestedState = nested.step()
            if (nestedState != RESULTS_AVAILABLE) {
                state = nestedState
                when (state) {
                    FAILED -> {
                        error = try {
                            nested.tryAdvance(); null
                        } catch (ex: Throwable) { ex }!!
                    }
                }
                return nestedState
            }

            val element: T = nested.tryAdvance()!!
            currentMapperSequence = buildLazySequence<M>(principal) { mapper(element) }
            inMapper = true
            return step()
        }
    }

    override tailrec fun tryAdvance(): M? {
        while (!inMapper && state != DEPLETED && state != FAILED) {
            step()
        }

        when (state) {
            DEPLETED -> return null
            FAILED -> throw error!!
            // implicit: inMapper == true, thus currentMapperSequence != null
            PENDING,
            RESULTS_AVAILABLE -> {
                val nextSubResult = currentMapperSequence!!.tryAdvance()
                step()

                if (nextSubResult == null) {
                    // depleted, next
                    return tryAdvance()
                } else {
                    return nextSubResult
                }
            }
        }
    }

    override fun close() {
        val mapperSequenceEx = try {
            currentMapperSequence?.close()
            null
        } catch (ex: Throwable) {
            ex
        }
        currentMapperSequence = null

        inMapper = false
        state = DEPLETED
        val nestedEx = try {
            nested.close()
            null
        } catch (ex: Throwable) {
            ex
        }

        throwMultipleNotNull(nestedEx, mapperSequenceEx)
    }
}
