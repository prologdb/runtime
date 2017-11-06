package com.github.tmarsteel.ktprolog.parser.lexer

import com.github.tmarsteel.ktprolog.parser.source.SourceLocation

class SourceLocationAwareCharIterator(private val initial: SourceLocation, private val source: Iterator<Char>) : Iterator<Pair<Char, SourceLocation>> {
    private var currentLine = initial.line
    private var currentColumn = initial.column
    private var sourceIndex = initial.sourceIndex

    private val currentLocation: SourceLocation
        get() = SourceLocation(initial.unit, currentLine, currentColumn, sourceIndex)

    override fun hasNext() = source.hasNext()

    override fun next(): Pair<Char, SourceLocation> {
        val char = source.next()
        val location = currentLocation

        if (char == '\n') {
            currentLine++
            currentColumn = 1
        }
        else {
            currentColumn++
        }
        sourceIndex++

        return Pair(char, location)
    }
}