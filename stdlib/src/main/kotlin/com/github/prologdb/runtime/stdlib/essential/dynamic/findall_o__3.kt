package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.launchWorkableFuture
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

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
 * * the third argument is a list and does not have a tail. Looks for `N+1` solutions where `N` is
 *   the number of elements in the list.
 *   This works because the lack of a tail requires the number of solutions to the query to be exactly
 *   the number of elements in the list. To disprove that, it suffices to proove that there are strictly
 *   less or more solutions than elements (regardless of how many solutions there actually are).
 *   If the number of solutions found exactly matches the number of elements, unification can proceed
 *   as usual; exposing the same behaviour as `findall/3`.
 */
val BuiltinFindAllOptimized3 = nativeRule("findall_o", 3) { args, context ->
    val templateInput = args[0]
    val goalInput = args.getQuery(1)
    val solutionInput = args[2]

    if (solutionInput is Variable) {
        // no optimization possible, same behaviour as findall/3
        BuiltinFindAll3.fulfill(this, args.raw, context)
    }
    else {
        solutionInput as? PrologList ?: throw ArgumentTypeError(2, solutionInput, PrologList::class.java, Variable::class.java)

        if (solutionInput.tail != null && solutionInput.tail != Variable.ANONYMOUS) {
            // this cannot be optimized, it requires all solutions for correct behaviour
            BuiltinFindAll3.fulfill(this, args.raw, context)
        }
        else {
            var nResultsToCalculate = solutionInput.elements.size
            // if the tail is the anonymous variable, we do not need more
            // if there is no tail, this needs to fail if there are not exactly
            // as many solutions as there are elements in solutionInput
            // calculating solutionInput.size+1 solutions suffices to make that
            // decision (implicitly when unifying resultList with solutionInput)
            if (solutionInput.tail == null) nResultsToCalculate++

            val resultList = await(launchWorkableFuture(principal) {
                val resultSequence = buildLazySequence<Unification>(principal) {
                    context.fulfillAttach(this, goalInput, VariableBucket())
                }
                    .limitRemaining(nResultsToCalculate.toLong())
                    .mapRemaining { solution ->
                        templateInput.substituteVariables(solution.variableValues.asSubstitutionMapper())
                    }

                foldRemaining(resultSequence, mutableListOf<Term>()) { l, t -> l.add(t); l }
            })

            val resultListUnified = solutionInput.unify(PrologList(resultList), context.randomVariableScope)
            return@nativeRule if (resultListUnified != null) {
                resultListUnified.variableValues.retainAll(templateInput.variables + solutionInput.variables)
                resultListUnified
            } else {
                null
            }
        }
    }
}
