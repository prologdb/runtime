package com.github.tmarsteel.ktprolog.parser.source

/**
 * A single unit of source code (e.g. a file, the contents of a textarea or a network command)
 */
open class SourceUnit(val identifier: String) {
    override fun toString() = identifier
}