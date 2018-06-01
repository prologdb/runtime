package com.github.prologdb.parser.lexer

import com.github.prologdb.parser.sequence.IteratorBasedTransactionalSequence
import com.github.prologdb.parser.sequence.TransactionalSequence
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.PrologException

class Lexer(source: Iterator<Char>, initialSourceLocation: SourceLocation) : TransactionalSequence<Token>
{
    private val lexerIteratorTxSequence = IteratorBasedTransactionalSequence(LexerIterator(source, initialSourceLocation))

    constructor(unit: SourceUnit, source: Iterator<Char>): this(source, SourceLocation(unit, 1, 0, 0))

    override fun mark() = lexerIteratorTxSequence.mark()

    override fun commit() = lexerIteratorTxSequence.commit()

    override fun hasNext() = lexerIteratorTxSequence.hasNext()

    override fun next() = lexerIteratorTxSequence.next()

    override fun rollback() = lexerIteratorTxSequence.rollback()
}

class LexerIterator(givenSource: Iterator<Char>, initialSourceLocation: SourceLocation) : Iterator<Token> {

    private val source: TransactionalSequence<Pair<Char, SourceLocation>>
    init {
        source = IteratorBasedTransactionalSequence(
            SourceLocationAwareCharIterator(
                initialSourceLocation,
                givenSource
            )
        )
    }

    private var next: Token? = findNext()

    override fun hasNext() = next != null

    override fun next(): Token {
        if (next == null) throw NoSuchElementException()

        val token = next!!
        next = findNext()
        return token
    }

    /**
     * @return The nextToken token from the input or `null` if there are no more tokens.
     */
    private fun findNext(): Token? {
        skipWhitespace()

        if (!source.hasNext()) return null

        // try to match an operator
        val operatorToken = tryMatchOperator()
        if (operatorToken != null) {
            // String
            if (operatorToken.operator == Operator.DOUBLE_QUOTE) {
                var stringContent = collectStringLike(startToken = operatorToken)
                return StringLiteralToken(stringContent.first, stringContent.second)
            }

            return operatorToken
        }

        if (!source.hasNext()) return null

        var text = collectUntilOperatorOrWhitespace()

        if (text.first[0].isDigit()) {
            // NUMERIC LITERAL
            val numberWithLocation: Pair<Number, SourceLocationRange>
            source.mark()

            if (source.hasNext()) {
                var next = source.next()
                if (next.first == DECIMAL_SEPARATOR && source.hasNext()) {
                    source.mark()
                    next = source.next()
                    if (next.first.isDigit()) {
                        // <DIGIT...> <SEPARATOR> <DIGIT...> => floating point literal
                        source.rollback()
                        val afterSeparator = collectUntilOperatorOrWhitespace()
                        val numberLocationRange = SourceLocationRange(text.second.start, afterSeparator.second.end)
                        val numberAsString = text.first + '.' + afterSeparator.first
                        numberWithLocation = Pair(numberAsString.toDouble(), numberLocationRange)
                    } else {
                        // <DIGIT...> <SEPARATOR> <!DIGIT> => treat the separator as an operator; the nextCharsAsString invocation will
                        // find it and deal with it
                        source.commit()
                        source.rollback()
                        numberWithLocation = Pair(text.first.toLong(), text.second)
                    }
                } else {
                    numberWithLocation = Pair(text.first.toLong(), text.second)
                    source.rollback()
                }
            } else {
                numberWithLocation = Pair(text.first.toLong(), text.second)
                source.commit()
            }

            return NumericLiteralToken(numberWithLocation.first, numberWithLocation.second)
        }
        else {
            // IDENTIFIER
            return IdentifierToken(text.first, text.second)
        }
    }

    /**
     * @return the next [n] characters as a string. If there are not as many characters in the source,
     * will return `null`.
     */
    private fun nextCharsAsString(n: Int): Pair<String, SourceLocationRange>? {
        if (n < 1) throw IllegalArgumentException()

        var start: SourceLocation? = null
        var end: SourceLocation? = null
        val charAr = CharArray(n)
        for (i in 0 until n) {
            if (source.hasNext()) {
                val next = source.next()
                charAr[i] = next.first

                if (start == null) {
                    start = next.second
                }

                end = next.second
            }
            else {
                return null
            }
        }

        return Pair(String(charAr), SourceLocationRange(start!!, end!!))
    }

    private fun tryMatchOperator(doCommit: Boolean = true): OperatorToken? {
        for (operator in Operator.values())
        {
            source.mark()

            val nextText = nextCharsAsString(operator.text.length)
            if (nextText != null && nextText.first == operator.text) {
                if (doCommit) source.commit() else source.rollback()
                return OperatorToken(operator, nextText.second)
            }

            source.rollback()
        }

        return null
    }

    private fun collectUntilOperatorOrWhitespace(): Pair<String, SourceLocationRange> {
        val buf = StringBuilder()
        var start: SourceLocation? = null
        var end: SourceLocation? = null

        while (source.hasNext()) {
            val operator = tryMatchOperator(false)
            if (operator != null) break

            source.mark()
            val next = source.next()

            if (IsWhitespace(next.first)) {
                source.rollback()
                break
            }

            if (start == null) {
                start = next.second
            }
            end = next.second

            source.commit()

            buf.append(next.first)
        }

        return Pair(buf.toString(), SourceLocationRange(start!!, end!!))
    }

    /**
     * Collects chars from the input as long as they fulfill [pred]. If a character is encountered that does not
     * fulfill the predicate, the collecting is stopped and the characters collected so far are returned. The character
     * causing the break can then be obtained using [nextCharsAsString].
     */
    private fun collectWhile(pred: (Char) -> Boolean): Pair<String, SourceLocationRange>?
    {
        var buf = StringBuilder()
        var start: SourceLocation? = null
        var end: SourceLocation? = null

        var charWithlocation: Pair<Char, SourceLocation>
        while (source.hasNext())
        {
            source.mark()
            charWithlocation = source.next()

            if (start == null) {
                start = charWithlocation.second
            }

            if (pred(charWithlocation.first)) {
                buf.append(charWithlocation.first)
                end = charWithlocation.second
                source.commit()
            }
            else {
                source.rollback()
                break
            }
        }

        if (start != null && end != null) {
            return Pair(buf.toString(), SourceLocationRange(start, end))
        }
        else {
            return null
        }
    }

    /**
     * Assumes the parser is right after an operator that introduces a string-like
     * sequence (actual string, escaped atom). Parses until it finds another instance
     * of the operator that is not escaped by the [ESCAPE_CHARACTER].
     *
     * @param startToken The operator that started the sequence. The same operator can end the sequence
     * @return The actual content (with escape sequences removed) and the [SourceLocationRange] of the
     *         entire sequence, including the start- and end operators.
     */
    private fun collectStringLike(startToken: OperatorToken): Pair<String, SourceLocationRange> {
        var stringContent = ""
        source.mark() // A
        while (true) {
            if (!source.hasNext()) throw PrologException("Unexpected EOF in string starting at ${startToken.location.start}")

            // look for escape sequence
            source.mark() // B
            val possibleEscapeChar = source.next()
            if (possibleEscapeChar.first == ESCAPE_CHARACTER) {
                // escape sequence found, take the next char as given
                if (!source.hasNext()) {
                    source.rollback() // B
                    source.rollback() // A
                    throw PrologException("Unexpected EOF after escape character in ${possibleEscapeChar.second}")
                }

                // if this is a special escape sequence, do a replacement
                val nextCharInSource = source.next().first
                val escapeSequenceConsideredChar = ESCAPE_SEQUENCES[nextCharInSource] ?: nextCharInSource

                stringContent += escapeSequenceConsideredChar
                continue
            } else {
                source.rollback() // B
            }

            // try to find ending operator
            source.mark() // B

            val possibleEndingOperatorText = nextCharsAsString(startToken.operator.text.length)
            if (possibleEndingOperatorText == null) {
                // not enough chars left for the ending delimiter => unexpected eof
                source.rollback() // B
                source.rollback() // A
                throw PrologException("Failed to parse string starting at ${startToken.location.start}: unexpected EOF, expected ${startToken.operator}")
            }

            if (possibleEndingOperatorText.first == startToken.operator.text) {
                // here is the ending delimiter, done
                source.commit() // B
                source.commit() // A
                return Pair(stringContent, startToken.location .. possibleEndingOperatorText.second.end)
            }

            source.rollback() // B
            stringContent += source.next().first // implicit commit
        }
    }

    private fun skipWhitespace() {
        collectWhile(IsWhitespace)
    }
}

private val IsWhitespace: (Char) -> Boolean = { c -> c == ' ' || c == '\t' || c == '\n' || c == '\r' }