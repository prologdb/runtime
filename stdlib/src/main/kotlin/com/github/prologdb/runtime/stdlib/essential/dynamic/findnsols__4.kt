package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.launchWorkableFuture
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

/**
 * findnsols(+N, @Template, :Goal, -List)
 * Like findall/3 but finds at most N solutions
 */
val BuiltinFindNSols4 = nativeRule("findnsols", 4) { args, context ->
    val nSolutions = args[0]
    val templateInput = args[1]
    val goalInput = args[2]
    val solutionInput = args[3]

    if (nSolutions !is PrologInteger) throw PrologRuntimeException("Type error: argument 1 to findnsols/4 must be an integer, got ${nSolutions.prologTypeName}")

    if (goalInput !is CompoundTerm) throw PrologRuntimeException("Type error: argument 3 to findnsols/4 must be a query.")

    if (solutionInput !is Variable && solutionInput !is PrologList) throw PrologRuntimeException("Type error: argument 4 to findnsols/4 must be unbound or a list, got ${solutionInput.prologTypeName}")

    val resultList = await(launchWorkableFuture(principal) {
        val resultSequence = buildLazySequence<Unification>(principal) {
            context.fulfillAttach(this, compoundToQuery(goalInput), VariableBucket())
        }
            .limitRemaining(nSolutions.value)
            .mapRemaining { solution ->
                templateInput.substituteVariables(solution.variableValues.asSubstitutionMapper())
            }

        foldRemaining(resultSequence, mutableListOf<Term>()) { l, t -> l.add(t); l }
    })

    val resultListUnified = solutionInput.unify(PrologList(resultList), context.randomVariableScope)
    resultListUnified?.variableValues?.retainAll(templateInput.variables + solutionInput.variables)
    return@nativeRule resultListUnified
}
