package com.github.prologdb.runtime.stdlib.essential.clauses

import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.proofsearch.PrologCallableFulfill
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable

val BuiltinError1 = object : Rule(CompoundTerm("error", arrayOf(Variable("Term"))), PredicateInvocationQuery(CompoundTerm("__nativeCode", emptyArray()))) {
    override val fulfill: PrologCallableFulfill = { args, ctxt ->
        val errorTerm = args[0]
        if (!errorTerm.isGround) {
            throw PrologInvocationContractViolationException("The error descriptor to error/1 must be ground")
        }

        throw Error1Exception(when(errorTerm) {
            is PrologString -> errorTerm.toKotlinString()
            else -> errorTerm.toString()
        })
    }
}

private class Error1Exception(message: String) : PrologException(message)