package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.runtime.PrologInvocationContractViolationException

/**
 * Thrown when a [Reductor] is defined in prolog, but incorrectly.
 */
class PrologReductorDefinitionException(
    val specification: PredicateReductor.Specification,
    message: String,
    cause: Throwable? = null
) : PrologInvocationContractViolationException(
    "Erroneous definition of reductor $specification: $message",
    cause
)