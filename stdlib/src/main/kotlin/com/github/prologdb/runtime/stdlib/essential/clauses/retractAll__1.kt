package com.github.prologdb.runtime.stdlib.essential.clauses

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.stdlib.nativeRule

val BuiltinRetractAll1 = nativeRule("retractAll", 1) { args, ctxt ->
    throw PrologRuntimeException("retract/1 is not implemented yet.")
}
