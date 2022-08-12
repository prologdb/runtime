package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.flatMapRemaining
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinReduce2 = nativeRule("reduce", 2) { args, ctxt ->
    val goal = args.getQuery(1)
    val reductionSpecifications = args.getTyped<PrologList>(0)
        .also {
            if (it.tail != null) {
                throw ArgumentError(0, "The list of reductors cannot have a tail")
            }
        }
        .elements
        .mapIndexed { index, specTerm -> parseSpecification(specTerm, 0, index) }

    val goalVariables = goal.variables
    val backReferences = reductionSpecifications.asSequence()
        .flatMap { it.resultTerm.variables }
        .filterNot { it is AnonymousVariable }
        .filter { it in goalVariables }
        .toList()

    if (backReferences.isNotEmpty()) {
        throw ArgumentError(1, "The source goal uses results of the reductors (${backReferences.joinToString(", ")}); this is not supported.")
    }

    val reductions = reductionSpecifications
        .map { spec ->
            val initResult = await(ReductionFactory.initializeReduction(ctxt, spec.reductorSpecification))
            val error = initResult.reportings.find { it.level >= Reporting.Level.ERROR }
            if (error != null || !initResult.isSuccess) {
                throw ArgumentError(0, "Failed to create reductor ${spec.reductorSpecification.toStringUsingOperatorNotations(ctxt.operators)}: $error")
            }
            ReductionWithResultTerm(await(initResult.item!!.create(ctxt)), spec.resultTerm)
        }

    yieldAll(
        buildLazySequence(ctxt.principal) {
            ctxt.fulfillAttach(this, goal, VariableBucket())
        }
            .flatMapRemaining { element ->
                for (reduction in reductions) {
                    await(reduction.reduction.add(element.variableValues))
                }
                // its mandatory not to yield anything. This allows the step() calls on the parent
                // sequence to trickle down to the reduction properly but not have the side-effect of yielding
                // unwanted solutions to reduce/2
                // the yieldAll is purely for the side-effects on the reductions
                return@flatMapRemaining null
            }
    )

    val resultOutTerms = reductions.map { it.resultTerm }.toTypedArray()
    val actualResultTerms = reductions.map { await(it.reduction.finalize()) }.toTypedArray()

    return@nativeRule resultOutTerms.unify(actualResultTerms, ctxt.randomVariableScope)
}

private fun parseSpecification(specification: Term, listArgumentIndex: Int, indexInList: Int): ReductionSpecification {
    val errorPrefix = "Invalid reductor (at index $indexInList):"
    var sourceInfoString: String
    when(specification.sourceInformation) {
        is SourceLocation -> sourceInfoString = " ${specification.sourceInformation}"
        else -> {
            sourceInfoString = ""
            specification.sourceInformation.sourceFileName?.let {
                sourceInfoString += " in $it"
            }
            specification.sourceInformation.sourceFileLine?.let {
                sourceInfoString += " on line $it"
            }
        }
    }

    if (specification !is CompoundTerm || specification.functor != "as" || specification.arity != 2) {
        throw ArgumentError(
            listArgumentIndex,
             "$errorPrefix reductions must be given as instances of as/2, e.g. as(count, NResults)$sourceInfoString",
        )
    }

    val reductorSpecification = specification.arguments[0]
    val resultVariable = specification.arguments[1]

    if (resultVariable !is Variable) {
        throw ArgumentError(
            listArgumentIndex,
            "$errorPrefix the result must be unbound",
        )
    }

    return ReductionSpecification(reductorSpecification, resultVariable)
}

private class ReductionSpecification(val reductorSpecification: Term, val resultTerm: Term)
private class ReductionWithResultTerm(val reduction: Reduction, val resultTerm: Term)