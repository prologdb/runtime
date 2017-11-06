package com.github.tmarsteel.ktprolog.parser

import com.github.tmarsteel.ktprolog.parser.lexer.Token
import com.github.tmarsteel.ktprolog.parser.source.SourceLocationRange

sealed class Reporting(val message: String) {
    override fun toString() = message
}

class UnexpectedTokenError(val actual: Token, vararg val expected: String) : Reporting("Unexpected $actual, expecting ${expected.joinToString(", ")}") {
    constructor(actual: Token, vararg expected: Enum<*>) : this(actual, *expected.map { it.name }.toTypedArray())

    override fun toString() = "$message in ${actual.location.start}"
}

class UnexpectedEOFError(vararg val expected: String): Reporting("Unexpected EOF, expecting ${expected.joinToString(", ")}") {
    constructor(vararg expected: Enum<*>) : this(*expected.map { it.name }.toTypedArray())
}

class SemanticError(message: String, val location: SourceLocationRange): Reporting(message) {
    override fun toString() = "$message in ${location.start}"
}