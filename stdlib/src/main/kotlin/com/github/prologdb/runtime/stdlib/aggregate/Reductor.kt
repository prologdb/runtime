package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.WorkableFuture
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.ParseResultCertainty
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.VariableBucket

/**
 * Implements a reduction (e.g. count, sum, average, ...).
 *
 * The methods in this interface must be stateless and thread-safe because a single instance will be used for many
 * reduction processes. All state required for a reduction must live in the [Accumulator].
 * [Reductor]s that use a mutable [Accumulator] must perform only thread-safe operations on the [Accumulator].
 *
 * This interface is semantically equivalent to a [java.util.stream.Collector]`<VariableBucket, ?, ? extends Term>`.
 */
interface Reductor<Specification, Accumulator, Result> {
    /**
     * Will be called to formalize the specification from prolog code.
     * @return `null` item with [ParseResultCertainty.NOT_RECOGNIZED] if the specification does not match this reductor.
     * any item with [ParseResultCertainty.MATCHED] if the specification matches this reductor. If there are any errors
     * in the [ParseResult.reportings] those will be reported back to the user.
     */
    fun parseSpecification(specification: Term): ParseResult<Specification>

    /**
     * Called once before aggregating. The carry is passed to all
     */
    fun initialize(ctxt: ProofSearchContext, specification: Specification): WorkableFuture<Accumulator>

    /**
     * Accumulates the given [element] into the [accumulator].
     * @return the result of the accumulation. If the reducer uses a mutable accumulator, then [accumulator] should be returned.
     */
    fun accumulate(ctxt: ProofSearchContext, accumulator: Accumulator, element: VariableBucket): WorkableFuture<Accumulator>

    /**
     * Is called after all elements have been accumulated.
     */
    fun finalize(ctxt: ProofSearchContext, accumulator: Accumulator): WorkableFuture<Result>

    fun resultToTerm(ctxt: ProofSearchContext, result: Result): Term
}