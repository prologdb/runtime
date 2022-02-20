package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologException

class InvalidMathOperatorInvocationException(val indicator: ClauseIndicator, val invocation: ClauseIndicator) : PrologException(
    "Operator $indicator cannot be invoked with an instance of $invocation"
)