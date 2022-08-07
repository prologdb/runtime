package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.WorkableFuture
import com.github.prologdb.async.map
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.ParseResultCertainty
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.VariableBucket
import java.util.ServiceLoader

object ReductionFactory {
    private val reductors = ServiceLoader.load(Reductor::class.java)

    fun reloadReductors() {
        reductors.reload()
    }

    fun initializeReduction(proofSearchContext: ProofSearchContext, specification: Term): WorkableFuture<ParseResult<Reduction>> {
        for (reductor in reductors) {
            val parseResult = reductor.parseSpecification(specification)

            if (parseResult.certainty == ParseResultCertainty.NOT_RECOGNIZED) {
                continue
            }

            @Suppress("UNCHECKED_CAST")
            val reduction = parseResult.item
                ?.let { parsedSpecification -> DefaultReduction.initialize<Any, Any, Any>(
                    proofSearchContext,
                    reductor as Reductor<Any, Any, Any>,
                    parsedSpecification,
                )}
                ?: WorkableFuture.completed(null)

            return reduction.map { ParseResult(it, parseResult.certainty, parseResult.reportings) }
        }

        // TODO: implement reduction implemented as prolog

        return WorkableFuture.completed(ParseResult(null, ParseResultCertainty.MATCHED, setOf(
            SemanticError(
                "There is no reductor for specification ${specification.toStringUsingOperatorNotations(proofSearchContext.operators)}",
                specification.sourceInformation as? SourceLocation ?: SourceLocation.EOF,
            )
        )))
    }
}

private class DefaultReduction<Specification, Accumulator, Result> private constructor(
    private val proofSearchContext: ProofSearchContext,
    private val reductor: Reductor<Specification, Accumulator, Result>,
    @Volatile
    private var accumulator: Accumulator,
) : Reduction {
    override suspend fun add(element: VariableBucket): WorkableFuture<Unit> {
        val previousAccumulator = accumulator
        return reductor.accumulate(proofSearchContext, accumulator, element)
            .map { newAccumulator ->
                if (newAccumulator !== previousAccumulator) {
                    accumulator = newAccumulator
                }
            }
    }

    override suspend fun finalize(): WorkableFuture<Term> {
        return reductor.finalize(proofSearchContext, accumulator)
            .map { reductor.resultToTerm(proofSearchContext, it) }
    }

    companion object {
        @JvmStatic
        fun <Specification, Accumulator, Result> initialize(
            proofSearchContext: ProofSearchContext,
            reductor: Reductor<Specification, Accumulator, Result>,
            specification: Specification,
        ): WorkableFuture<DefaultReduction<Specification, Accumulator, Result>> {
            return reductor.initialize(proofSearchContext, specification)
                .map { initialAccumulator -> DefaultReduction(proofSearchContext, reductor, initialAccumulator) }
        }
    }
}