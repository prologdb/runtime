package com.github.prologdb.runtime.builtin.dynamic

import com.github.prologdb.async.mapRemaining
import com.github.prologdb.async.remainingToList
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable

internal val BuiltinFindAll = prologBuiltin("findall", 3) { args, context ->
    val templateInput = args[0]
    val goalInput = args[1]
    val solutionInput = args[2]

    if (goalInput !is Predicate) throw PrologRuntimeException("Type error: second argument to findall/3 must be a predicate.")

    if (solutionInput !is Variable && solutionInput !is PrologList) {
        throw PrologRuntimeException("Type error: third argument to findall/3 must be a list or not instantiated.")
    }

    val resultList = context.knowledgeBase.fulfill(goalInput, context)
        .mapRemaining { solution ->
            templateInput.substituteVariables(solution.variableValues.asSubstitutionMapper())
        }
        .remainingToList()

    val resultListUnified = solutionInput.unify(PrologList(resultList), context.randomVariableScope)
    if (resultListUnified != null) {
        resultListUnified.variableValues.retainAll(templateInput.variables + solutionInput.variables)
        yield(resultListUnified)
    }
}