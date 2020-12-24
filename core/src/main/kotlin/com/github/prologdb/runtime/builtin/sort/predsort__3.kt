package com.github.prologdb.runtime.builtin.sort

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.essential.dynamic.BuiltinApply2
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

private val Delta = Variable("Delta")

internal val BuiltinPredsort3 = nativeRule("predsort", 3) { args, ctxt ->
    val prologComparator = args[0]
    val unsorted = args[1] as? PrologList
        ?: throw PrologRuntimeException("Argument 2 to predsort/3 must be a list, got ${args[1].prologTypeName}")
    val sorted = args[2]

    if (unsorted.tail != null) {
        throw PrologRuntimeException("Argument 2 to predsort/3 must not have a tail")
    }

    val comparator = Comparator<Term> { termA, termB ->
        val comparatorResultSequence = buildLazySequence<Unification>(ctxt.principal) {
            BuiltinApply2.fulfill(this, arrayOf(prologComparator, PrologList(listOf(Delta, termA, termB))), ctxt)
        }

        val firstResult = comparatorResultSequence.tryAdvance()
        comparatorResultSequence.close()

        if (firstResult == null) {
            throw PrologRuntimeException("Comparator predicate did not yield a solution.")
        }

        if (!firstResult.variableValues.isInstantiated(Delta)) {
            throw PrologRuntimeException("Comparator predicate did not instantiate first argument")
        }

        val delta = firstResult.variableValues[Delta] as? Atom
            ?: throw PrologRuntimeException("Comparator predicate must instantiate first argument to either <, = or >")

        when (delta.name) {
            "<" -> -1
            "=" -> 0
            ">" -> 1
            else -> throw PrologRuntimeException("Comparator predicate must instantiate first argument to either <, = or >")
        }
    }

    val sortedElements = unsorted.elements.sortedWith(comparator)

    return@nativeRule PrologList(sortedElements).unify(sorted, ctxt.randomVariableScope)
}
