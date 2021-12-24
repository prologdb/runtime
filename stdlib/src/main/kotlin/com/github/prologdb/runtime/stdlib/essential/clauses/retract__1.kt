package com.github.prologdb.runtime.stdlib.essential.clauses

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.proofsearch.DynamicPrologPredicate
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.CompoundTerm

val BuiltinRetract1 = nativeRule("retract", 1) { args, ctxt ->
    val clauseTemplate = args.getTyped<CompoundTerm>(0)

    val (fqIndicator, callable, retractableClause) = ctxt.resolveHead(clauseTemplate)

    if (callable !is DynamicPrologPredicate) {
        throw PrologRuntimeException("Predicate $fqIndicator is not dynamic")
    }

    return@nativeRule callable.retract(this, retractableClause, ctxt)
}
