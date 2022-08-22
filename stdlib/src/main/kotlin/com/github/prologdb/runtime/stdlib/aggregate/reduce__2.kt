package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.flatMapRemaining
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.stdlib.ReductionFactory
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.UnificationGrouping
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinReduce2 = nativeRule("reduce", 2) { args, ctxt ->
    val (goal, existentialVariables) = args.getQueryWithExistentialVariables(1)
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

    val reductionFactories = reductionSpecifications
        .map { spec ->
            val initResult = await(Reductors.initializeReduction(ctxt, spec.reductorSpecification))
            val error = initResult.reportings.find { it.level >= Reporting.Level.ERROR }
            if (error != null || !initResult.isSuccess) {
                throw ArgumentError(0, "Failed to create reductor ${spec.reductorSpecification.toStringUsingOperatorNotations(ctxt.operators)}: $error")
            }
            ReductionFactoryWithResultTerm(initResult.item!!, spec.resultTerm)
        }

    val specificationVariables = reductionSpecifications
        .flatMap { it.reductorSpecification.variables }
        .toSet()
    val groupByVariables = goalVariables - existentialVariables - specificationVariables
    val grouping = UnificationGrouping<List<Reduction>>(ctxt.randomVariableScope)

    await(
        buildLazySequence(ctxt.principal) {
            ctxt.fulfillAttach(this, goal, VariableBucket())
        }
            .flatMapRemaining { element ->
                val groupKey = element.getGroupKey(groupByVariables)
                var reductions = grouping[groupKey]
                if (reductions == null) {
                    reductions = ArrayList<Reduction>(reductionFactories.size)
                    for (factory in reductionFactories) {
                        reductions.add(await(factory.reductionFactory.create(ctxt)))
                    }
                    grouping[groupKey] = reductions
                }

                for (reduction in reductions) {
                    await(reduction.add(element.variableValues))
                }
            }
            .consumeAll()
    )

    val resultOutTerms = reductionFactories.map { it.resultTerm }.toTypedArray()

    for ((groupKey, reductions) in grouping) {
        val actualResultTerms = reductions.map { await(it.finalize()) }.toTypedArray()
        val resultUnification = resultOutTerms.unify(actualResultTerms, ctxt.randomVariableScope)
        resultUnification
            ?.combinedWith(Unification(groupKey), ctxt.randomVariableScope)
            ?.let { yield(it) }
    }

    return@nativeRule Unification.FALSE
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
private class ReductionFactoryWithResultTerm(val reductionFactory: ReductionFactory, val resultTerm: Term)

private fun Unification.getGroupKey(keys: Set<Variable>): VariableBucket {
    if (keys.isEmpty()) {
        return VariableBucket()
    }

    return variableValues.copy().apply {
        retainAll(keys)
    }
}