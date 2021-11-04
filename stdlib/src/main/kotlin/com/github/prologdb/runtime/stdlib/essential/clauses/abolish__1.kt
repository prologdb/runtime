package com.github.prologdb.runtime.stdlib.essential.clauses

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologPermissionError
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.unification.Unification

val BuiltinAbolish1 = nativeRule("abolish", 1) { args, ctxt ->
    val arg0 = args[0]
    if (arg0 !is CompoundTerm || arg0.arity != 2 || arg0.functor != "/") throw PrologRuntimeException("Argument 0 to abolish/1 must be an instance of `/`/2")
    if (arg0.arguments[0] !is Atom || arg0.arguments[1] !is PrologInteger) throw PrologRuntimeException("Argument 0 to abolish/1 must be an indicator")

    val name = (arg0.arguments[0] as Atom).name
    val arity = (arg0.arguments[1] as PrologInteger).value.toInt()

    val (fqIndicator, _) = ctxt.resolveCallable(ClauseIndicator.of(name, arity))
        ?: return@nativeRule Unification.FALSE

    if (!ctxt.authorization.mayWrite(fqIndicator)) {
        throw PrologPermissionError("Not allowed to write $fqIndicator")
    }

    throw PrologRuntimeException("abolish/1 is not fully implemented yet.")
}
