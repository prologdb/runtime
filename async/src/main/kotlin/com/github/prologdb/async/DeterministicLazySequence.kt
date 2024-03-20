package com.github.prologdb.async

import com.github.prologdb.async.LazySequence.State.DEPLETED
import com.github.prologdb.async.LazySequence.State.FAILED
import com.github.prologdb.async.LazySequence.State.PENDING
import com.github.prologdb.async.LazySequence.State.RESULTS_AVAILABLE

/**
 * Assures that the wrapped [LazySequence] returns exactly one result and that the state is [LazySequence.State.DEPLETED]
 * after retrieving that result. Throws an exception if that is not the case.
 *
 * This class can also be used to assert semi-determinism, by passing `null` for [onNoResults].
 */
class DeterministicLazySequence<T : Any>(
    private val base: LazySequence<T>,
    private val onUnfinishedAfterResult: () -> Nothing,
    private val onSecondResult: () -> Nothing,
    /**
     * If `null`, the resulting sequence will allow semi-determinism (up to one result, state DEPLETED afterwards).
     */
    private val onNoResults: (() -> Nothing)?,
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

            when (val baseState = base.step()) {
                DEPLETED -> {
                    val ex = try {
                        onNoResults
                            ?.let { it() }
                            ?: return DEPLETED
                    } catch (ex: Throwable) {
                        ex
                    }
                    error = ex
                    return FAILED
                }
                else -> return baseState
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

            when (val baseState = base.state) {
                DEPLETED -> {
                    val ex = try {
                        onNoResults
                            ?.let { it() }
                            ?: return DEPLETED
                    } catch (ex: Throwable) {
                        ex
                    }
                    error = ex
                    return FAILED
                }
                else -> return baseState
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
                    val ex = try {
                        onNoResults
                            ?.let { it() }
                            ?: return null
                    } catch (ex: Throwable) {
                        ex
                    }
                    error = ex
                    throw ex
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
                PENDING -> error("logically unreachable. IntelliJ realizes this but Kotlin 1.9.0 doesn't allow me to omit this branch.")
            }
        }
    }

    override fun close() {
        base.close()
    }
}