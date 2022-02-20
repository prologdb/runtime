package com.github.prologdb.runtime.stdlib.essential.math

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologException

class UndefinedMathOperatorException(val indicator: ClauseIndicator) : PrologException("Arithmetic operator $indicator is not defined")