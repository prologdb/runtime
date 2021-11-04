package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

val Term.asPrologNumber: PrologNumber
    get() = when (this) {
        is PrologNumber -> this
        is Variable -> throw PrologRuntimeException("Arguments not sufficiently instantiated: $this")
        is CompoundTerm -> MathOperatorRegistry.evaluate(this)
        else -> throw PrologRuntimeException("expected number, got ${this.prologTypeName}")
    }
