package com.github.prologdb.runtime.builtin.essential.clauses

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativeRule

val BuiltinRetract1 = nativeRule("retract", 1) { args, ctxt ->
    throw PrologRuntimeException("retract/1 is not implemented yet.")
}
