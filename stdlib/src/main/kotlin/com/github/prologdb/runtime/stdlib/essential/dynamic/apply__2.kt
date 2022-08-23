package com.github.prologdb.runtime.stdlib.essential.dynamic

import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.stdlib.nativeRule
import com.github.prologdb.runtime.term.PrologList

val BuiltinApply2 = nativeRule("apply", 2) { args, ctxt ->
    val arguments = args.getTyped<PrologList>(1)
    if (arguments.tail != null) {
        throw ArgumentError(1, "must not have a tail")
    }

    args.getCallable(0, arguments.elements.size, ctxt).fulfill(this, arguments.elements.toTypedArray(), ctxt)
}
