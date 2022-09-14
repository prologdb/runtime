package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.launchWorkableFuture
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

/**
 * findnsols(+N, @Template, :Goal, -List)
 * Like findall/3 but finds at most N solutions
 */
val BuiltinFindNSols4 = nativeRule("findnsols", 4) { args, context ->
    val nSolutions = try {
        args.getInteger(0).toInteger()
    } catch (_: ArithmeticException) {
        throw PrologInvocationContractViolationException("Cannot find more than ${Int.MAX_VALUE} solutions with findnsols/4")
    }
    if (nSolutions > Int.MAX_VALUE) {
        throw PrologInvocationContractViolationException("Cannot find more than ${Int.MAX_VALUE} solutions with findnsols/4")
    }
    val templateInput = args[1]
    val goalInput = args.getQuery(2)
    val solutionInput = args[3]

    if (solutionInput !is Variable && solutionInput !is PrologList) {
        throw ArgumentTypeError(3, solutionInput, Variable::class.java, PrologList::class.java)
    }

    val resultList = await(launchWorkableFuture(principal) {
        val resultSequence = buildLazySequence<Unification>(principal) {
            context.fulfillAttach(this, goalInput, Unification.TRUE)
        }
            .limitRemaining(nSolutions)
            .mapRemaining { solution ->
                templateInput.substituteVariables(solution.asSubstitutionMapper())
            }

        foldRemaining(resultSequence, mutableListOf<Term>()) { l, t -> l.add(t); l }
    })

    if (resultList.size != nSolutions.toInt()) {
        return@nativeRule Unification.FALSE
    }

    val resultListUnified = solutionInput.unify(PrologList(resultList), context.randomVariableScope)
    return@nativeRule resultListUnified
        ?.createMutableCopy()
        ?.apply {
            retainAll(templateInput.variables + solutionInput.variables)
        }
}
