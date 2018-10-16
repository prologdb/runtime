package com.github.prologdb.async

/**
 * A lazy sequence where the calculation of results can be done
 * step by step where the steps, ideally, are at boundaries between
 * CPU/memory bound tasks and I/O bound tasks. This aims to optimize
 * resource utilization in a situation where there are large numbers
 * of [WorkableLazySequence]s open at the same time.
 *
 * Calling [tryAdvance] on a [WorkableLazySequence] will perform
 * **all** necessary steps to calculate the next result (or determin
 * that there are no more). For more fine grained control/information,
 * use [step] and [state].
 */
interface WorkableLazySequence<T> : LazySequence<T> {
    /**
     * Performs CPU&memory bound work for the next element in the
     * sequence and returns when waiting for I/O bound tasks
     * (disk, memory, ...).
     *
     * When this method is called in state [State.RESULTS_AVAILABLE],
     * it is not defined whether calculations are done.
     *
     * @return the state of this sequence after this call
     */
    fun step(): State

    val state: State

    enum class State {
        /**
         * No results are immediately available. Either applies:
         * * the next step is also CPU&memory bound and calling [step] again would
         *   advance the process of calculating the solution
         * * the next step is I/O bound and is waiting for that to finish. In this case,
         *   calling [step] again does not have any effect.
         */
        PENDING,

        /**
         * Results are available for immediate grab without calculation
         * from [tryAdvance]. After grabbing all results the sequence will
         * transition to any of the other states.
         */
        RESULTS_AVAILABLE,

        /**
         * All results have been calculated and obtained via [tryAdvance]
         */
        DEPLETED,

        /**
         * An error occured while calculating the next result.
         * [tryAdvance] will throw that error.
         */
        FAILED
    }
}