package com.github.tmarsteel.ktprolog.parser.source

import com.github.tmarsteel.ktprolog.parser.sequence.TransactionalSequence

/**
 * A location in a unit of source code (e.g. a source file)
 */
class SourceLocation (
        val unit: SourceUnit,

        /** The line number, starting with 1 */
        val line: Int,

        /** The column number, starting with 1 */
        val column: Int,

        val charIndex: Int
) : TransactionalSequence.Companion.SequencePosition {
    override val index = charIndex
}