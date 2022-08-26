package com.github.prologdb.parser

import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.PrologException

sealed class Reporting(val level: Level, val message: String, val location: SourceLocation) {
    override fun toString() = "$message in $location"

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Reporting) return false

        if (level != other.level) return false
        if (message != other.message) return false
        if (location != other.location) return false

        return true
    }

    override fun hashCode(): Int {
        var result = level.hashCode()
        result = 31 * result + message.hashCode()
        result = 31 * result + location.hashCode()
        return result
    }


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

class UnexpectedEOFError(vararg val expected: String): Reporting(Level.ERROR, "Unexpected EOF, expecting ${expected.joinToString(" or ")}", SourceLocationRange.EOF) {
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
            val highestLevel = reportings.maxOfOrNull { it.level } ?: return
            if (highestLevel < Reporting.Level.ERROR) {
                return
            }

            val highestLevelReportings = reportings.filter { it.level == highestLevel }
            val reportingMessage = highestLevelReportings.joinToString(separator = "\n")

            val message = if (extraMessage != null) "$extraMessage: $reportingMessage" else reportingMessage
            throw ParseException(highestLevelReportings.first(), message, cause)
        }
    }
}