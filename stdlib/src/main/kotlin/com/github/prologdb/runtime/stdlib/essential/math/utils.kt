package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.InsufficientInstantiationException
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.MathContext
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

fun Term.evaluateAsMathematicalExpression(context: MathContext): PrologNumber = when (this) {
    is PrologNumber -> this
    is Variable -> throw InsufficientInstantiationException(this)
    is CompoundTerm -> MathOperatorRegistry.evaluate(this, context)
    else -> throw PrologInvocationContractViolationException("expected number, got ${this.prologTypeName}")
}
