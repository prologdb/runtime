package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.*
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.ParseResultCertainty
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.PredicateNotDefinedException
import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.PrologInternalError
import com.github.prologdb.runtime.module.ModuleScopeProofSearchContext
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import java.util.concurrent.atomic.AtomicReference

/**
 * Bridges reductors defined in prolog to the [Reductor] interface. Here is how to implement a reductor in prolog:
 *
 * A reductor is composed of three predicates. To make name resolution easier, they are all combined into one
 * for this purpose and an atom as an argument is used to distinguish between the three predicates.
 *
 * Here are the contracts for the three accumulator functions (all must be deterministic):
 *
 * ```prolog
 * % Specification is the specification from the invocation of reduce/2. If reduce/2 was called like so:
 * % reduce([my_module:custom(A) as Result], Goal).
 * % then Specification will be custom(A)
 * reductorName(reductor, initialize, +Specification, -InitialAccumulator)
 *
 * % the specification is instantiated with the variables from the solution being accumulated
 * % instantiates AccumulatorOut with the result of adding InstantiatedSpecification to AccumulatorIn
 * reductorName(reductor, accumulate, +InstantiatedSpecification, +AccumulatorIn, -AccumulatorOut)
 *
 * % the specification is not instantiated (as in initialize)
 * % AccumulatorIn is the last accumulator obtained from the accumulate function
 * % Instantiates ResultOut to the result of the reduction
 * reductorName(reductor, finalize, +Specification, +AccumulatorIn, -ResultOut)
 * ```
 *
 * This is an implementation of the `min` reductor.
 *
 * ```prolog
 * min(reductor, initialize, min(_), {initial: true}).
 *
 * min(reductor, accumulate, min(Of), {initial: true}, {acc: Of}).
 * min(reductor, accumulate, min(Of), {acc: Acc}, {acc: Acc}) :- Of >= Acc.
 * min(reductor, accumulate, min(Of), {acc: Acc}, {acc: Of}) :- Of > Acc.
 *
 * min(reductor, finalize, min(_), {initial: true}, _).
 * min(reductor, finalize, min(_), {acc: Min}, Min).
 * ```
 */
class PredicateReductor : Reductor<PredicateReductor.Specification, PredicateReductor.Accumulator, Term> {
    override fun parseSpecification(ctxt: ProofSearchContext, specification: Term): WorkableFuture<ParseResult<Specification>> {
        var reductorModule: String?
        val unqualifiedSpecification: Term

        if (specification is CompoundTerm && specification.functor == ":" && specification.arity == 2) {
            val moduleTerm = specification.arguments[0]
            if (moduleTerm !is Atom) {
                return WorkableFuture.completed(ParseResult(null, ParseResultCertainty.MATCHED, setOf(
                    SemanticError(
                        "Prolog reductors must be qualified by an instance of `:`/2 with an atom as the module name.",
                        specification.arguments[0].sourceInformation as? SourceLocation ?: SourceLocation.EOF
                    )
                )))
            }
            reductorModule = moduleTerm.name
            unqualifiedSpecification = specification.arguments[1]
        } else {
            reductorModule = null
            unqualifiedSpecification = specification
        }

        val reductorName = when(unqualifiedSpecification) {
            is Atom -> unqualifiedSpecification.name
            is CompoundTerm -> unqualifiedSpecification.functor
            else -> return WorkableFuture.completed(ParseResult(null, ParseResultCertainty.MATCHED, setOf(
                SemanticError(
                    "Prolog reductors must have an atom or a compound term as the specifier, got ${specification.prologTypeName}",
                    unqualifiedSpecification.sourceInformation as? SourceLocation ?: SourceLocation.EOF,
                )
            )))
        }

        reductorModule = reductorModule
            ?: (ctxt as? ModuleScopeProofSearchContext)?.module?.declaration?.moduleName
            ?: throw PrologInternalError("Cannot find implicit module for reductor $reductorName")

        val parsedSpecification = Specification(reductorModule, unqualifiedSpecification, reductorName)
        return launchWorkableFuture(ctxt.principal) {
            try {
                await(initialize(ctxt, parsedSpecification))
            }
            catch (ex: PrologReductorDefinitionException) {
                return@launchWorkableFuture ParseResult(null, ParseResultCertainty.NOT_RECOGNIZED, emptySet())
            }
            catch (ex: Exception) {
                throw ex
            }

            ParseResult.of(parsedSpecification)
        }
    }

    override fun initialize(ctxt: ProofSearchContext, specification: Specification): WorkableFuture<Accumulator> {
        return launchWorkableFuture(ctxt.principal) {
            val initialAccumulatorVar = ctxt.randomVariableScope.createNewRandomVariable()
            try {
                await(ctxt.solveDeterministic(
                    {
                        ctxt.fulfillAttach(
                            this,
                            specification.buildInvocation(
                                ATOM_REDUCTOR,
                                ATOM_INITIALIZE,
                                specification.specificationTerm,
                                initialAccumulatorVar,
                            ),
                            VariableBucket(),
                        )
                    },
                    { PrologReductorDefinitionException(specification, "initialization yielded no solutions.") },
                    { PrologReductorDefinitionException(specification, "initialization yielded more than one solution") }
                )
                    .map {
                        val initialAccumulatorTerm = it.variableValues[initialAccumulatorVar].substituteVariables(it.variableValues.asSubstitutionMapper())
                        Accumulator(specification, initialAccumulatorTerm)
                    }
                )
            }
            catch (ex: PredicateNotDefinedException) {
                if (ex.indicator.functor == specification.reductorName && ex.indicator.arity == 4 && ex.inContextOfModule.declaration.moduleName == specification.module) {
                    throw PrologReductorDefinitionException(specification, "Reductor ${specification.reductorName} is not defined", ex)
                }

                throw ex
            }
        }
    }

    override fun accumulate(
        ctxt: ProofSearchContext,
        accumulator: Accumulator,
        element: VariableBucket
    ): WorkableFuture<Accumulator> {
        return accumulator.accumulateOnSelf(ctxt, element).map { accumulator }
    }

    override fun finalize(ctxt: ProofSearchContext, accumulator: Accumulator): WorkableFuture<Term> {
        return accumulator.finalize(ctxt)
    }

    override fun resultToTerm(ctxt: ProofSearchContext, result: Term): Term = result

    class Specification(val module: String, val specificationTerm: Term, val reductorName: String) {
        fun buildInvocation(vararg args: Term): PredicateInvocationQuery {
            return PredicateInvocationQuery(
                CompoundTerm(":", arrayOf(Atom(module), CompoundTerm(reductorName, args)))
            )
        }

        override fun toString() = "$module:$specificationTerm"
    }
    class Accumulator(
        val specification: Specification,

        /**
         * The result of calling the initialization goal of the reductor.
         */
        initialAccumulator: Term
    ) {
        private val accumulatorTerm = AtomicReference(initialAccumulator)

        fun accumulateOnSelf(ctxt: ProofSearchContext, element: VariableBucket): WorkableFuture<Unit> {
            val instantiatedSpecification = specification.specificationTerm.substituteVariables(element.asSubstitutionMapper())
            val accumulatorTermIn = accumulatorTerm.getAcquire()
            val accumulatorTermOutVariable = ctxt.randomVariableScope.createNewRandomVariable()

            return ctxt
                .solveDeterministic(
                    {
                        ctxt.fulfillAttach(
                            this,
                            specification.buildInvocation(
                                ATOM_REDUCTOR,
                                ATOM_ACCUMULATE,
                                instantiatedSpecification,
                                accumulatorTermIn,
                                accumulatorTermOutVariable,
                            ),
                            VariableBucket(),
                        )
                    },
                    { PrologReductorDefinitionException(specification, "accumulate yielded zero solutions") },
                    { PrologReductorDefinitionException(specification, "accumulate yielded more than one solution") },
                )
                .map {
                    val newAccumulatorTerm = it.variableValues[accumulatorTermOutVariable].substituteVariables(it.variableValues.asSubstitutionMapper())
                    if (!accumulatorTerm.compareAndSet(accumulatorTermIn, newAccumulatorTerm)) {
                        throw PrologInternalError("Concurrent actions on an ongoing reduction")
                    }
                }
        }

        fun finalize(ctxt: ProofSearchContext): WorkableFuture<Term> {
            val resultTermOutVariable = ctxt.randomVariableScope.createNewRandomVariable()

            return ctxt
                .solveDeterministic(
                    {
                        ctxt.fulfillAttach(
                            this,
                            specification.buildInvocation(
                                ATOM_REDUCTOR,
                                ATOM_FINALIZE,
                                specification.specificationTerm,
                                accumulatorTerm.getAcquire(),
                                resultTermOutVariable,
                            ),
                            VariableBucket(),
                        )
                    },
                    { PrologReductorDefinitionException(specification, "finalize yielded zero solutions") },
                    { PrologReductorDefinitionException(specification, "finalize yielded more than one solution") },
                )
                .map {
                    it.variableValues[resultTermOutVariable].substituteVariables(it.variableValues.asSubstitutionMapper())
                }
        }
    }

    private companion object {
        val ATOM_REDUCTOR = Atom("reductor")
        val ATOM_INITIALIZE = Atom("initialize")
        val ATOM_ACCUMULATE = Atom("accumulate")
        val ATOM_FINALIZE = Atom("finalize")
    }
}

private fun ProofSearchContext.solveDeterministic(
    goal: suspend LazySequenceBuilder<Unification>.() -> Unification?,
    onZero: () -> PrologException,
    onMultiple: () -> PrologException,
): WorkableFuture<Unification> {
    return launchWorkableFuture(this.principal) {
        val result = foldRemaining<Unification, Unification?>(buildLazySequence(this.principal, goal), null) { carry, solution ->
            if (carry != null) {
                throw onMultiple()
            }
            solution
        }

        return@launchWorkableFuture result ?: throw onZero()
    }
}