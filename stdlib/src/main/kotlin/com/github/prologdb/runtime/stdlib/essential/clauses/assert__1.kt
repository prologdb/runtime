package com.github.prologdb.runtime.stdlib.essential.clauses

import com.github.prologdb.parser.lexer.Operator
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PredicateNotDynamicException
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.proofsearch.DynamicPrologPredicate
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification

val BuiltinAssert1 = nativeRule("assert", 1) { args, ctxt ->
    val clause = args.getTyped<CompoundTerm>(0)

    val fqIndicator: FullyQualifiedClauseIndicator
    val callable: PrologCallable
    val assertableClause: Clause

    if (clause.functor == Operator.HEAD_QUERY_SEPARATOR.text && clause.arity == 2) {
        if (clause.arguments[0] is CompoundTerm) {
            throw PrologRuntimeException("Asserting rules is not supported.")
        }
    }

    val resolved = ctxt.resolveHead(clause)
    fqIndicator = resolved.first
    callable = resolved.second
    assertableClause = resolved.third

    if (callable is DynamicPrologPredicate) {
        callable.assertz(assertableClause)
        return@nativeRule Unification.TRUE
    } else {
        throw PredicateNotDynamicException(fqIndicator)
    }
}
