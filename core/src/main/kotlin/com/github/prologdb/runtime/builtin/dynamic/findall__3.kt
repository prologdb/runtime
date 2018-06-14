package com.github.prologdb.runtime.builtin.dynamic

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.lazysequence.mapRemaining
import com.github.prologdb.runtime.lazysequence.remainingToList
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.List as PrologList

val BuiltinFindAll = prologBuiltin("findall", 3) { args, knowledgeBase, randomVarsScope ->
    val templateInput = args[0]
    val goalInput = args[1]
    val solutionInput = args[2]

    if (goalInput !is Predicate) throw PrologRuntimeException("Type error: second argument to findall/3 must be a predicate.")

    if (solutionInput !is Variable && solutionInput !is PrologList) {
        throw PrologRuntimeException("Type error: third argument to findall/3 must be a list or not instantiated.")
    }

    val resultList = knowledgeBase.fulfill(goalInput, randomVarsScope).mapRemaining { solution ->
        templateInput.substituteVariables(solution.variableValues.asSubstitutionMapper())
    }.remainingToList()

    val resultListUnified = solutionInput.unify(PrologList(resultList), randomVarsScope)
    resultListUnified?.variableValues?.retainAll(templateInput.variables + solutionInput.variables)

    return@prologBuiltin LazySequence.ofNullable(resultListUnified)
}