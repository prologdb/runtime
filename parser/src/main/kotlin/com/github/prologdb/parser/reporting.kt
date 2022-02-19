package com.github.prologdb.parser

import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange

sealed class Reporting(val level: Level, val message: String, val location: SourceLocation) {
    override fun toString() = "$message in $location"

    enum class Level {
        INFO,
        WARNING,
        ERROR
    }
}

class UnexpectedTokenError(val actual: Token, vararg val expected: String) : Reporting(Level.ERROR, "Unexpected $actual, expecting ${expected.joinToString(", ")}", actual.location) {
    constructor(actual: Token, vararg expected: Enum<*>) : this(actual, *expected.map { it.name }.toTypedArray())

    override fun toString() = "$message in ${actual.location.start}"
}

class UnexpectedEOFError(vararg val expected: String): Reporting(Level.ERROR, "Unexpected EOF, expecting ${expected.joinToString(", ")}", SourceLocationRange.EOF) {
    constructor(vararg expected: Enum<*>) : this(*expected.map { it.name }.toTypedArray())

    override fun toString() = message
}

class SemanticError(message: String, location: SourceLocation): Reporting(Level.ERROR, message, location)

class SyntaxError(message: String, location: SourceLocation): Reporting(Level.ERROR, message, location)

class SemanticWarning(message: String, location: SourceLocation): Reporting(Level.WARNING, message, location)

class SemanticInfo(message: String, location: SourceLocation): Reporting(Level.INFO, message, location)

open class ReportingException(val reporting: Reporting, cause: Throwable? = null) : Exception(reporting.message, cause)