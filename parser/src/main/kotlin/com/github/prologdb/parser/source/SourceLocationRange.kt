package com.github.prologdb.parser.source

import com.github.prologdb.runtime.PrologSourceInformation

class SourceLocationRange(val start: SourceLocation, val end: SourceLocation) : SourceLocation(start.unit, start.line, start.column, start.sourceIndex), PrologSourceInformation {
    init {
        if (start.unit != end.unit) {
            throw IllegalArgumentException("The two given locations must have the same source unit.")
        }

        if (start.line > end.line || (start.line == end.line && start.column > end.column)) {
            throw IllegalArgumentException("The start must be before the end.")
        }
    }

    override operator fun rangeTo(other: SourceLocation): SourceLocationRange {
        if (other is SourceLocationRange) {
            return SourceLocationRange(this, other.end)
        }
        else {
            return SourceLocationRange(this, other)
        }
    }

    override val sourceFileName: String = unit.identifier
    override val sourceFileLine: Int? = start.line

    companion object {
        val EOF = SourceLocationRange(SourceLocation.EOF, SourceLocation.EOF)
    }
}