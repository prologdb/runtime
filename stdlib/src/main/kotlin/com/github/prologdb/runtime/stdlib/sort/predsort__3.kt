package com.github.prologdb.runtime.stdlib.sort

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinApply2
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

private val Delta = Variable("Delta")

internal val BuiltinPredsort3 = nativeRule("predsort", 3) { args, ctxt ->
    val prologComparator = args[0]
    val unsorted = args.getTyped<PrologList>(1)
    val sorted = args[2]

    if (unsorted.tail != null) {
        throw ArgumentError(1, "must not have a tail")
    }

    val comparator = Comparator<Term> { termA, termB ->
        val comparatorResultSequence = buildLazySequence<Unification>(ctxt.principal) {
            BuiltinApply2.fulfill(this, arrayOf(prologComparator, PrologList(listOf(Delta, termA, termB))), ctxt)
        }

        val firstResult = comparatorResultSequence.tryAdvance()
        comparatorResultSequence.close()

        if (firstResult == null) {
            throw PrologInvocationContractViolationException("Comparator predicate did not yield a solution.")
        }

        if (!firstResult.variableValues.isInstantiated(Delta)) {
            throw PrologInvocationContractViolationException("Comparator predicate did not instantiate first argument")
        }

        val delta = firstResult.variableValues[Delta] as? Atom
            ?: throw PrologInvocationContractViolationException("Comparator predicate must instantiate first argument to either <, = or >")

        when (delta.name) {
            "<" -> -1
            "=" -> 0
            ">" -> 1
            else -> throw PrologInvocationContractViolationException("Comparator predicate must instantiate first argument to either <, = or >")
        }
    }

    val sortedElements = unsorted.elements.sortedWith(comparator)

    return@nativeRule PrologList(sortedElements).unify(sorted, ctxt.randomVariableScope)
}
