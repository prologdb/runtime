package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.WorkableFuture
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.ParseResultCertainty
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.InsufficientInstantiationException
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

class SetReductor : Reductor<Term, SetReductor.Accumulator, PrologList> {
    override fun parseSpecification(ctxt: ProofSearchContext, specification: Term): WorkableFuture<ParseResult<Term>> {
        if (specification !is CompoundTerm || specification.functor != NAME) {
            return WorkableFuture.completed(ParseResult(null, ParseResultCertainty.NOT_RECOGNIZED, emptySet()))
        }

        if (specification.arity != 1) {
            return WorkableFuture.completed(ParseResult(null, ParseResultCertainty.MATCHED, setOf(SemanticError(
                "The $NAME reductor takes exactly one argument, got ${specification.arity}",
                specification.sourceInformation as? SourceLocation ?: SourceLocation.EOF
            ))))
        }

        val template = specification.arguments[0]
        if (template.variables.isEmpty()) {
            return WorkableFuture.completed(ParseResult(template, ParseResultCertainty.MATCHED, setOf(SemanticError(
                "the term doesn't have free variables, so reduction won't do any meaningful work",
                template.sourceInformation as? SourceLocation ?: SourceLocation.EOF,
            ))))
        }

        return WorkableFuture.completed(ParseResult.of(template))
    }

    override fun initialize(ctxt: ProofSearchContext, specification: Term): WorkableFuture<Accumulator> {
        return WorkableFuture.completed(Accumulator(specification, Collections.newSetFromMap(ConcurrentHashMap())))
    }

    override fun accumulate(
        ctxt: ProofSearchContext,
        accumulator: Accumulator,
        element: Unification
    ): WorkableFuture<Accumulator> {
        val instantiatedTemplate = accumulator.template.substituteVariables(element.asSubstitutionMapper())
        if (instantiatedTemplate.variables.isNotEmpty()) {
            throw InsufficientInstantiationException(
                instantiatedTemplate.variables.first(),
                "Template ${accumulator.template} for $NAME reductor is not ground after instantiating with $element"
            )
        }

        accumulator.values.add(instantiatedTemplate)
        return WorkableFuture.completed(accumulator)
    }

    override fun finalize(ctxt: ProofSearchContext, accumulator: Accumulator): WorkableFuture<PrologList> {
        return WorkableFuture.completed(PrologList(accumulator.values.toList(), null))
    }

    override fun resultToTerm(ctxt: ProofSearchContext, result: PrologList) = result

    companion object {
        const val NAME = "set"
    }

    class Accumulator(val template: Term, val values: MutableSet<Term>)
}