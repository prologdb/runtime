package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.WorkableFuture
import com.github.prologdb.async.launchWorkableFuture
import com.github.prologdb.async.map
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.ParseResultCertainty
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.stdlib.ReductionFactory
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.VariableBucket
import java.util.ServiceLoader

object Reductors {
    private val reductors = ServiceLoader.load(Reductor::class.java)
    private val predicateReductor = PredicateReductor()

    /**
     * Refreshes the list of available reductors from the classpath
     */
    fun reloadReductors() {
        reductors.reload()
    }

    fun initializeReduction(proofSearchContext: ProofSearchContext, specification: Term): WorkableFuture<ParseResult<ReductionFactory>> {
        return launchWorkableFuture(proofSearchContext.principal) {
            for (reductor in reductors + listOf(predicateReductor)) {
                val parseResult = await(reductor.parseSpecification(proofSearchContext, specification))

                if (parseResult.certainty == ParseResultCertainty.NOT_RECOGNIZED) {
                    continue
                }

                if (parseResult.item != null) {
                    return@launchWorkableFuture parseResult.map { parsedSpecification ->
                        @Suppress("UNCHECKED_CAST")
                        DefaultReductionFactory(reductor as Reductor<Any, Any, Any>, parsedSpecification as Any)
                    }
                }
            }

            return@launchWorkableFuture ParseResult(
                null,
                ParseResultCertainty.MATCHED,
                setOf(
                    SemanticError(
                        "There is no reductor for specification ${
                            specification.toStringUsingOperatorNotations(
                                proofSearchContext.operators
                            )
                        }",
                        specification.sourceInformation as? SourceLocation ?: SourceLocation.EOF,
                    )
                )
            )
        }
    }
}

private class DefaultReductionFactory<Specification, Accumulator, Result>(
    private val reductor: Reductor<Specification, Accumulator, Result>,
    private val specification: Specification,
) : ReductionFactory {
    override fun create(proofSearchContext: ProofSearchContext) = launchWorkableFuture(proofSearchContext.principal) {
        DefaultReduction(proofSearchContext, reductor, await(reductor.initialize(proofSearchContext, specification)))
    }
}

private class DefaultReduction<Specification, Accumulator, Result>(
    private val proofSearchContext: ProofSearchContext,
    private val reductor: Reductor<Specification, Accumulator, Result>,
    @Volatile
    private var accumulator: Accumulator,
) : Reduction {
    override fun add(element: VariableBucket): WorkableFuture<Unit> {
        val previousAccumulator = accumulator
        return reductor.accumulate(proofSearchContext, accumulator, element)
            .map { newAccumulator ->
                if (newAccumulator !== previousAccumulator) {
                    accumulator = newAccumulator
                }
            }
    }

    override fun finalize(): WorkableFuture<Term> {
        return reductor.finalize(proofSearchContext, accumulator)
            .map { reductor.resultToTerm(proofSearchContext, it) }
    }
}