package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.*
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.ParseResultCertainty
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.PrologInternalError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
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
    override fun parseSpecification(specification: Term): ParseResult<Specification> {
        val moduleName: String?
        val unqualifiedSpecification: Term

        if (specification is CompoundTerm && specification.functor == ":" && specification.arity == 2) {
            val moduleTerm = specification.arguments[0] as? Atom ?: return ParseResult(null, ParseResultCertainty.MATCHED, setOf(
                SemanticError(
                    "Prolog reductors must be qualified by an instance of `:`/2 with an atom as the module name.",
                    specification.arguments[0].sourceInformation as? SourceLocation ?: SourceLocation.EOF
                )
            ))
            moduleName = moduleTerm.name
            unqualifiedSpecification = specification.arguments[1]
        } else {
            moduleName = null
            unqualifiedSpecification = specification
        }

        val reductorName = when(specification) {
            is Atom -> specification.name
            is CompoundTerm -> specification.functor
            else -> return ParseResult(null, ParseResultCertainty.MATCHED, setOf(
                SemanticError(
                    "Prolog reductors must have an atom or a compound term as the specifier, got ${specification.prologTypeName}",
                    specification.sourceInformation as? SourceLocation ?: SourceLocation.EOF,
                )
            ))
        }

        return ParseResult.of(Specification(moduleName, unqualifiedSpecification, reductorName))
    }

    override fun initialize(ctxt: ProofSearchContext, specification: Specification): WorkableFuture<Accumulator> {
        val reductorModule = specification.module
            ?: (ctxt as? ModuleScopeProofSearchContext)?.module?.declaration?.moduleName
            ?: throw PrologInternalError("Cannot find implicit module for reductor ${specification.reductorName}")

        val initialAccumulatorVar = ctxt.randomVariableScope.createNewRandomVariable()
        return ctxt.solveDeterministic(
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
            { PrologInvocationContractViolationException("Initialization of reductor failed: $reductorModule:${specification.reductorName} yielded no solutions.") },
            { PrologInvocationContractViolationException("Initialization of reductor failed: $reductorModule:${specification.reductorName} yielded more than one solution") }
        )
            .map {
                val initialAccumulatorTerm = it.variableValues[initialAccumulatorVar].substituteVariables(it.variableValues.asSubstitutionMapper())
                Accumulator(
                    specification,
                    reductorModule,
                    initialAccumulatorTerm
                )
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

    class Specification(val module: String?, val specificationTerm: Term, val reductorName: String) {
        fun buildInvocation(vararg args: Term): PredicateInvocationQuery {
            var goal = CompoundTerm(reductorName, args)
            if (module != null) {
                goal = CompoundTerm(":", arrayOf(Atom(module), goal))
            }

            return PredicateInvocationQuery(goal)
        }
    }
    class Accumulator(
        val specification: Specification,
        val reductorModule: String,

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
                    { PrologInvocationContractViolationException("Reductor $reductorModule:${specification.reductorName} yielded zero solutions on accumulate.") },
                    { PrologInvocationContractViolationException("Reductor $reductorModule:${specification.reductorName} yielded more than one solution on accumulate.") },
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
                    { PrologInvocationContractViolationException("Reductor $reductorModule:${specification.reductorName} yielded zero solutions on finalize.") },
                    { PrologInvocationContractViolationException("Reductor $reductorModule:${specification.reductorName} yielded more than one solution on finalize.") },
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