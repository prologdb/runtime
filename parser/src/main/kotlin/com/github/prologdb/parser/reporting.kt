package com.github.prologdb.parser

import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange

sealed class Reporting(val message: String, val location: SourceLocation) {
    override fun toString() = message
}

class UnexpectedTokenError(val actual: Token, vararg val expected: String) : Reporting("Unexpected $actual, expecting ${expected.joinToString(", ")}", actual.location) {
    constructor(actual: Token, vararg expected: Enum<*>) : this(actual, *expected.map { it.name }.toTypedArray())

    override fun toString() = "$message in ${actual.location.start}"
}

class UnexpectedEOFError(vararg val expected: String): Reporting("Unexpected EOF, expecting ${expected.joinToString(", ")}", SourceLocationRange.EOF) {
    constructor(vararg expected: Enum<*>) : this(*expected.map { it.name }.toTypedArray())
}

class SemanticError(message: String, location: SourceLocationRange): Reporting(message, location) {
    override fun toString() = "$message in $location"
}

class SyntaxError(message: String, location: SourceLocation): Reporting(message, location) {
    override fun toString() = "$message in $location"
}

open class ReportingException(val reporting: Reporting, cause: Throwable? = null) : Exception(reporting.message, cause)