package com.github.tmarsteel.ktprolog.parser

import com.github.tmarsteel.ktprolog.parser.lexer.Token

sealed class Reporting(val message: String)

class UnexpectedTokenError(val actual: Token, vararg val expected: String) : Reporting("Unexpected $actual, expecting ${expected.joinToString(", ")}") {
    constructor(actual: Token, vararg expected: Enum<*>) : this(actual, *expected.map { it.name }.toTypedArray())
}

class UnexpectedEOFError(vararg val expected: String): Reporting("Unexpected EOF, expecting ${expected.joinToString(", ")}") {
    constructor(vararg expected: Enum<*>) : this(*expected.map { it.name }.toTypedArray())
}