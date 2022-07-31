package com.github.prologdb.parser.source

/**
 * A location in a unit of source code (e.g. a source file)
 */
open class SourceLocation (
    val unit: SourceUnit,

    /** The line number, starting with 1 */
    val line: Int,

    /** The column number, starting with 1 */
    val column: Int,

    /** The index in the source input of characters */
    val sourceIndex: Int
) {
    override fun toString() = "$unit:$line, column $column"

    open operator fun rangeTo(other: SourceLocation): SourceLocationRange {
        if (this.unit != other.unit) {
            throw IllegalArgumentException("The stat and end of a source location range must be in the same source unit")
        }

        return if (other is SourceLocationRange) {
            rangeTo(other.end)
        } else {
            when {
                this.line < other.line      -> SourceLocationRange(this, other)
                other.line < this.line      -> SourceLocationRange(other, this)
                this.column <= other.column -> SourceLocationRange(this, other)
                else                        -> SourceLocationRange(other, this)
            }
        }
    }

    companion object {
        val EOF = object : SourceLocation(SourceUnit(""), -1, -1, -1) {
        override fun toString() = "EOF"
        }
    }
}