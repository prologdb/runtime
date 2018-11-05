package com.github.prologdb.runtime.builtin.dynamic

import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.prologBuiltin
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

/**
 * findall/3 always finds all solutions and then unifies a list of those with the input. This
 * builtin only finds as many solutions as are required to yield the same result (excluding
 * side effects); if the goal is free of side effects, this behaves exactly the same as findall/3
 * but possibly uses less resources.
 *
 * These scenarios are optimized:
 *
 * * the third argument is a list, has a tail and that tail is the anonymous variable. Looks only
 *   for as much solutions as there are elements in the list.
 *   Say `a(N)` has 5 solutions then `findall(N, a(N), [A|_])` finds all 5 solutions and instantiates
 *   `A` to the first one found. `findall_o(N, a(N), [A|_])` starts the proof search. As soon as the
 *   first solution is found, the proof search is aborted and the resources are released. `A` is then,
 *   equally as before, instantiated to the first solution.
 */
internal val BuiltinFindAllOptimized = prologBuiltin("findall_o", 3) { args, context ->
    val templateInput = args[0]
    val goalInput = args[1]
    val solutionInput = args[2]

    if (goalInput !is Predicate) throw PrologRuntimeException("Type error: second argument to findall_o/3 must be a query.")

    if (solutionInput is Variable) {
        // no optimization possible, same behaviour as findall/3
        yieldAll(context.knowledgeBase.fulfill(Predicate("findall", args), context))
    }

    solutionInput as? PrologList ?: throw PrologRuntimeException("Type error: third argument to findall_o/3 must be a list or not instantiated.")

    if (solutionInput.tail == null || solutionInput.tail != Variable.ANONYMOUS) {
        // this cannot be optimized, it requires all solutions for correct behaviour
        context.knowledgeBase.fulfill(Predicate("findall", args), context)
        return@prologBuiltin
    }

    val resultSequence = context.knowledgeBase.fulfill(predicateToQuery(goalInput), context).mapRemaining { solution ->
        templateInput.substituteVariables(solution.variableValues.asSubstitutionMapper())
    }

    val resultList = mutableListOf<Term>()
    for (i in 0 until solutionInput.elements.size) {
        resultList.add(resultSequence.tryAdvance() ?: break)
    }

    resultSequence.close()

    val resultListUnified = solutionInput.unify(PrologList(resultList), context.randomVariableScope)
    if (resultListUnified != null) {
        resultListUnified.variableValues.retainAll(templateInput.variables + solutionInput.variables)
        yield(resultListUnified)
    }
}