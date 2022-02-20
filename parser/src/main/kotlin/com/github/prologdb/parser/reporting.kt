package com.github.prologdb.parser

import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.PrologException

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

open class ParseException private constructor(val reporting: Reporting, message: String, cause: Throwable? = null) : PrologException(message, cause) {
    companion object {
        fun ofSingle(single: Reporting, extraMessage: String? = null, cause: Throwable? = null) = ParseException(
            single,
            if (extraMessage != null) "$extraMessage: $single" else single.toString(),
            cause
        )

        fun failOnError(reportings: Collection<Reporting>, extraMessage: String? = null, cause: Throwable? = null) : Unit {
            val mostSevere = reportings.maxByOrNull { it.level } ?: return
            if (mostSevere.level < Reporting.Level.ERROR) {
                return
            }

            var reportingMessage = mostSevere.toString()
            if (reportings.size > 1) {
                reportingMessage += " (and ${reportings.size - 1} more)"
            }

            val message = if (extraMessage != null) "$extraMessage: $reportingMessage" else reportingMessage
            throw ParseException(mostSevere, message, cause)
        }
    }
}