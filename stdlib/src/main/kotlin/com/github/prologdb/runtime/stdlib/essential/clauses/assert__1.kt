package com.github.prologdb.runtime.stdlib.essential.clauses

import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PredicateNotDynamicException
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.proofsearch.DynamicPrologPredicate
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification

val BuiltinAssert1 = nativeRule("assert", 1) { args, ctxt ->
    val clause = args[0] as? CompoundTerm ?: throw PrologRuntimeException("Argument 0 to assert/1 must be a compound term")

    val fqIndicator: FullyQualifiedClauseIndicator
    val callable: PrologCallable
    val assertableClause: Clause

    val fqTerm = ctxt.resolveModuleScopedCallable(clause)
    if (fqTerm != null) {
        fqIndicator = fqTerm.first
        callable = fqTerm.second
        assertableClause = CompoundTerm(callable.functor, fqTerm.third)
    } else {
        val simpleIndicator = ClauseIndicator.of(clause)

        val resolved = ctxt.resolveCallable(simpleIndicator)
            ?: throw PrologRuntimeException("Predicate $simpleIndicator is not known in $ctxt")

        fqIndicator = resolved.first
        callable = resolved.second
        assertableClause = if (fqIndicator.indicator.functor == clause.functor) clause else CompoundTerm(
            fqIndicator.indicator.functor,
            clause.arguments
        )
    }

    if (callable is DynamicPrologPredicate) {
        callable.assertz(assertableClause)
        return@nativeRule Unification.TRUE
    } else {
        throw PredicateNotDynamicException(fqIndicator)
    }
}
