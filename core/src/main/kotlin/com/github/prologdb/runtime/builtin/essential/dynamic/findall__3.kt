package com.github.prologdb.runtime.builtin.essential.dynamic

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.async.remainingToList
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

internal val BuiltinFindAll = nativeRule("findall", 3) { args, context ->
    val templateInput = args[0]
    val goalInput = args[1]
    val solutionInput = args[2]

    if (goalInput !is CompoundTerm) throw PrologRuntimeException("Type error: second argument to findall/3 must be a query.")

    if (solutionInput !is Variable && solutionInput !is PrologList) {
        throw PrologRuntimeException("Type error: third argument to findall/3 must be a list or not instantiated.")
    }

    val resultList = buildLazySequence<Unification>(principal) {
        context.fulfillAttach(this, compoundToQuery(goalInput), VariableBucket())
    }
        .mapRemaining { solution ->
            templateInput.substituteVariables(solution.variableValues.asSubstitutionMapper())
        }
        .remainingToList()

    val resultListUnified = solutionInput.unify(PrologList(resultList), context.randomVariableScope)
    return@nativeRule if (resultListUnified != null) {
        resultListUnified.variableValues.retainAll(templateInput.variables + solutionInput.variables)
        resultListUnified
    } else null
}

