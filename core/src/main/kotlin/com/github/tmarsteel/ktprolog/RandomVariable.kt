package com.github.tmarsteel.ktprolog

import com.github.tmarsteel.ktprolog.term.Variable

class RandomVariable(private val counter: Int) : Variable("_G" + counter)