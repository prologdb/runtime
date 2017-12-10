package com.github.prologdb.runtime

import com.github.prologdb.runtime.term.Variable

class RandomVariable(private val counter: Long) : Variable("_G" + counter)