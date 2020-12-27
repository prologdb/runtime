package com.github.prologdb.runtime.builtin.essential.clauses

import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PredicateNotDynamicException
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule
import com.github.prologdb.runtime.proofsearch.DynamicPrologPredicate
import com.github.prologdb.runtime.unification.Unification

val BuiltinAssert1 = nativeRule("assert", 1) { args, ctxt ->
    val clause = args[0] as? Clause ?: throw PrologRuntimeException("Argument 0 to assert/1 must be a clause")

    val simpleIndicator = ClauseIndicator.of(clause)

    val (fqIndicator, callable) = ctxt.resolveCallable(simpleIndicator)
        ?: throw PrologRuntimeException("Predicate $simpleIndicator is not known")

    if (callable is DynamicPrologPredicate) {
        callable.assertz(clause)
        return@nativeRule Unification.TRUE
    } else {
        throw PredicateNotDynamicException(fqIndicator)
    }
}
