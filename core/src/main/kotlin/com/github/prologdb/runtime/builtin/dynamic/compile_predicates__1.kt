package com.github.prologdb.runtime.builtin.dynamic

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.nativePredicate
import com.github.prologdb.runtime.term.PrologList

internal val BuiltinCompilePredicates = nativePredicate("compile_predicates", 1) { args, context ->
    val arg0 = args[0]
    if (arg0 !is PrologList) {
        throw PrologRuntimeException("Argument 1 to compile_predicates/1 must be a list, ${arg0.prologTypeName} given")
    }

    if (arg0.tail != null) {
        throw PrologRuntimeException("Argument 1 to compile_predicates/1 must not have a tail")
    }

    val indicators = arg0.elements.mapIndexed { index, term ->
        ClauseIndicator.ofIdiomatic(term, "element ${index + 1} in argument 1 to compile_predicates/1")
    }

    TODO()
}
