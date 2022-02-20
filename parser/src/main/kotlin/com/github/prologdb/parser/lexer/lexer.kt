package com.github.prologdb.parser.lexer

import com.github.prologdb.parser.ReportingException
import com.github.prologdb.parser.UnexpectedEOFError
import com.github.prologdb.parser.sequence.IteratorBasedTransactionalSequence
import com.github.prologdb.parser.sequence.TransactionalSequence
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.parser.source.SourceUnit

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

        // skip all the comments
        // this is done first because it makes sure the following lexer code gets in contact with actual source code to
        // lex (instead of getting screwed by weird combinations of comments and valid tokens, e.g.:
        // r /* ads */ :- /*b*/ c(_). % foo
        while (trySkipComment()) {}

        if (!source.hasNext()) return null

        // try to match an operator
        val operatorToken = tryMatchOperator()
        if (operatorToken != null) {
            // String
            if (operatorToken.operator == Operator.DOUBLE_QUOTE || operatorToken.operator == Operator.BACKTICK) {
                val stringContent = collectQuoted(startToken = operatorToken)
                return StringLiteralToken(stringContent.first, stringContent.second)
            }
            if (operatorToken.operator == Operator.SINGLE_QUOTE) {
                val atomContent = collectQuoted(startToken = operatorToken)
                return AtomLiteralToken(atomContent.first, true, atomContent.second)
            }

            return operatorToken
        }

        if (!source.hasNext()) return null

        val text = collectUntilOperatorOrWhitespace()

        if (text.first.isEmpty()) {
            // the next operator is straight ahead: jump back
            return findNext()
        }

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

    /**
     * Attempts to find a comment. If one is found, it is consumed from [source] and ignored and true is returned.
     * If none is found, [source] is left in the same position it was in when this method was invoked and false is
     * returned.
     */
    private fun trySkipComment(): Boolean  {
        // yes, a single if-statement with short-circuit OR behaves the same. However, that would make a
        // crucial part of this functions logic implicit. That would be bad.

        if (trySkipSingleLineComment()) return true
        if (trySkipMultiLineComment()) return true
        return false
    }

    /**
     * Attempts to find a single line comment. If one is found, the rest of the line is consumed and ignored and
     * true is returned.
     * If none is found, [source] is left in the position it was in when this method was invoked and false is returned.
     */
    private fun trySkipSingleLineComment(): Boolean {
        for (initSignal in SINGLE_LINE_COMMENT_SINGALS) {
            source.mark()
            val content = nextCharsAsString(initSignal.length)
            if (content == null) {
                source.rollback()
                continue
            }

            if (content.first == initSignal) {
                source.commit()
                collectWhile { it != '\n' }
                return true
            }

            source.rollback()
        }

        return false
    }

    /**
     * Attempts to find a multi-line comment. If one is found, the entire comment is consumed and ignored and
     * true is returned.
     * If none is found, [source] is left in the same position it was in when this method was invoked and false is
     * returned.
     */
    private fun trySkipMultiLineComment(): Boolean {
        for ((initSignal, endSignal) in MULTI_LINE_COMMENT_SIGNALS.entries) {
            source.mark() // A
            val content = nextCharsAsString(initSignal.length)
            if (content == null) {
                source.rollback() // A
                continue
            }

            if (content.first == initSignal) {
                // comment found; find the end
                source.commit() // A
                while (true) {
                    source.mark() // B
                    val possibleCommentEnd = nextCharsAsString(endSignal.length)
                    if (possibleCommentEnd == null) {
                        // the file ends within the comment => done
                        while (source.hasNext()) source.next()
                        source.commit() // B
                        return true
                    }

                    if (possibleCommentEnd.first == endSignal) {
                        source.commit() // B
                        return true
                    }

                    source.rollback() // B

                    source.next() // immediately commits
                }
            }
            else {
                // look ahead did not match
                source.rollback() // A
            }
        }

        return false
    }

    private fun collectUntilOperatorOrWhitespace(): Pair<String, SourceLocationRange> {
        if (!source.hasNext()) throw IllegalStateException("This method must be invoked with at least on character left in the source.")

        val buf = StringBuilder()
        var start: SourceLocation? = null
        var end: SourceLocation? = null

        while (source.hasNext()) {
            val operator = tryMatchOperator(false)
            if (operator != null) {
                if (start == null) start = operator.location.start
                if (end == null) end = operator.location.end

                break
            }

            source.mark()
            val next = source.next()

            if (IsWhitespace(next.first)) {
                source.rollback()

                if (start == null) start = next.second
                if (end == null) end = next.second

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
        val buf = StringBuilder()
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
    private fun collectQuoted(startToken: OperatorToken): Pair<String, SourceLocationRange> {
        var stringContent = ""
        source.mark() // A
        while (true) {
            if (!source.hasNext()) {
                throw ReportingException.ofSingle(UnexpectedEOFError("Unexpected EOF in string starting at ${startToken.location.start}"))
            }

            // look for escape sequence
            source.mark() // B
            val possibleEscapeChar = source.next()
            if (possibleEscapeChar.first == ESCAPE_CHARACTER) {
                // escape sequence found, take the next char as given
                if (!source.hasNext()) {
                    source.rollback() // B
                    source.rollback() // A
                    throw ReportingException.ofSingle(UnexpectedEOFError("Unexpected EOF after escape character in ${possibleEscapeChar.second}"))
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
                throw ReportingException.ofSingle(UnexpectedEOFError("Failed to parse string starting at ${startToken.location.start}: unexpected EOF, expected ${startToken.operator}"))
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
