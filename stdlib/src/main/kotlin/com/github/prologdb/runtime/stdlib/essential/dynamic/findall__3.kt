package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.async.remainingToList
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

val BuiltinFindAll3 = nativeRule("findall", 3) { args, context ->
    val templateInput = args[0]
    val goalInput = args.getQuery(1)
    val solutionInput = args[2]

    if (solutionInput !is Variable && solutionInput !is PrologList) {
        throw ArgumentTypeError(2, solutionInput, PrologList::class.java, Variable::class.java)
    }

    val resultList = await(
        buildLazySequence(principal) {
            context.fulfillAttach(this, goalInput, Unification.TRUE)
        }
            .mapRemaining { solution ->
                templateInput.substituteVariables(solution.asSubstitutionMapper())
            }
            .remainingToList()
    )

    return@nativeRule solutionInput.unify(PrologList(resultList), context.randomVariableScope)
        ?.subset(templateInput.variables + solutionInput.variables)
}

