package com.github.prologdb.parser.source

class SourceLocationRange(val start: SourceLocation, val end: SourceLocation) : SourceLocation(start.unit, start.line, start.column, start.sourceIndex) {
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

    companion object {
        val EOF = SourceLocationRange(SourceLocation.EOF, SourceLocation.EOF)
    }
}