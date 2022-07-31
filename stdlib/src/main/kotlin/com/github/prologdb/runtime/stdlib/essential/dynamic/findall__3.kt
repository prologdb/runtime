package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.async.remainingToList
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

val BuiltinFindAll3 = nativeRule("findall", 3) { args, context ->
    val templateInput = args[0]
    val goalInput = args.getTyped<CompoundTerm>(1)
    val solutionInput = args[2]

    if (solutionInput !is Variable && solutionInput !is PrologList) {
        throw ArgumentTypeError(2, solutionInput, PrologList::class.java, Variable::class.java)
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

