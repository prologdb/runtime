package com.github.prologdb.runtime.builtin.math

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

internal val Term.asPrologNumber: PrologNumber
    get() = when(this) {
        is PrologNumber -> this
        is Variable -> throw PrologRuntimeException("is/2: Arguments not sufficiently instantiated: $this")
        is Predicate -> MathOperatorRegistry.evaluate(this)
        else -> throw PrologRuntimeException("is/2: expected number, got ${this.prologTypeName}")
    }