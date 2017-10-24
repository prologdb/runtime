package com.github.tmarsteel.ktprolog.parser.source

class SourceLocationRange(val start: SourceLocation, val end: SourceLocation) : SourceLocation(start.unit, start.line, start.column) {
    init {
        if (start.unit != end.unit) {
            throw IllegalArgumentException("The two given locations must have the same source unit.")
        }

        if (start.line > end.line || (start.line == end.line && start.column > end.column)) {
            throw IllegalArgumentException("The start must be before the end.")
        }
    }
}