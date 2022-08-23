package com.github.prologdb.async

import com.github.prologdb.async.LazySequence.State.*

/**
 * Assures that the wrapped [LazySequence] returns exactly one result and that the state is [LazySequence.State.DEPLETED]
 * after retrieving that result. Throws an exception if that is not the case.
 */
class DeterministicLazySequence<T : Any>(
    private val base: LazySequence<T>,
    private val onUnfinishedAfterResult: () -> Nothing,
    private val onSecondResult: () -> Nothing,
    private val onNoResults: () -> Nothing,
) : LazySequence<T> {
    private var resultSeen: Boolean = false
    private var error: Throwable? = null
    private val mutex = Any()

    override val principal: Principal = base.principal

    override fun step(): LazySequence.State {
        if (error != null) {
            return FAILED
        }

        if (resultSeen) {
            return DEPLETED
        }

        synchronized(mutex) {
            if (error != null) {
                return FAILED
            }

            if (resultSeen) {
                return DEPLETED
            }

            return when (val baseState = base.step()) {
                DEPLETED -> {
                    val ex = try {
                        onNoResults()
                    } catch (ex: Throwable) {
                        ex
                    }
                    error = ex
                    FAILED
                }
                else -> baseState
            }
        }
    }

    override val state: LazySequence.State get() {
        if (error != null) {
            return FAILED
        }

        if (resultSeen) {
            return DEPLETED
        }

        synchronized(mutex) {
            if (error != null) {
                return FAILED
            }

            if (resultSeen) {
                return DEPLETED
            }

            return when (val baseState = base.state) {
                DEPLETED -> {
                    val ex = try {
                        onNoResults()
                    } catch (ex: Throwable) {
                        ex
                    }
                    error = ex
                    FAILED
                }
                else -> baseState
            }
        }
    }

    override fun tryAdvance(): T? {
        error?.let { throw it }

        if (resultSeen) {
            return null
        }

        synchronized(mutex) {
            error?.let { throw it }

            if (resultSeen) {
                return null
            }

            var baseState = base.state
            while (baseState == PENDING) {
                baseState = base.step()
            }

            when (baseState) {
                DEPLETED -> {
                    try {
                        onNoResults()
                    }
                    catch (ex: Throwable) {
                        error = ex
                        throw ex
                    }
                }
                FAILED -> return base.tryAdvance() // must throw
                RESULTS_AVAILABLE -> {
                    val result = base.tryAdvance()
                        ?: throw IllegalStateException("Solution sequence reported ${LazySequence.State.RESULTS_AVAILABLE} but did not return a value on tryAdvance")

                    resultSeen = true

                    when (base.state) {
                        DEPLETED -> return result
                        PENDING, FAILED -> {
                            try {
                                onUnfinishedAfterResult()
                            }
                            catch (ex: Throwable) {
                                error = ex
                                throw ex
                            }
                        }
                        RESULTS_AVAILABLE -> {
                            try {
                                onSecondResult()
                            }
                            catch (ex: Throwable) {
                                error = ex
                                throw ex
                            }
                        }
                    }
                }
                PENDING -> error("logically unreachable. IntelliJ realizes this but Kotlin 1.7.10 doesn't allow me to omit this branch.")
            }
        }
    }

    override fun close() {
        base.close()
    }
}