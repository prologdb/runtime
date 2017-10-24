package com.github.tmarsteel.ktprolog.parser.lexer

/**
 * Normalizes line-endings from `\r`, `\n` and `\r\n` to `\n`
 */
class LineEndingNormalizer(private val source: Iterator<Char>) : Iterator<Char> {

    private var peek: Char? = null

    private var nextChar: Char? = null

    private fun findNext() {
        nextChar = null

        if (peek == '\r') {
            nextChar = '\n'
            peek = null

            if (source.hasNext()) {
                peek = source.next()

                if (peek == '\n') {
                    // \r\n

                    peek = null
                    if (source.hasNext()) {
                        peek = source.next()
                    }
                }
            }

            return
        }

        nextChar = peek

        peek = null
        if (source.hasNext()) {
            peek = source.next()
        }
    }

    init {
        if (source.hasNext()) {
            peek = source.next()
            findNext()
        }
    }

    override fun hasNext() = nextChar != null

    override fun next(): Char {
        if (nextChar == null) {
            throw NoSuchElementException()
        }

        val tmp = nextChar!!
        findNext()
        return tmp
    }
}