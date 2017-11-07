package com.github.tmarsteel.ktprolog

import com.github.tmarsteel.ktprolog.term.Variable

class RandomVariable(private val counter: Long) : Variable("_G" + counter)