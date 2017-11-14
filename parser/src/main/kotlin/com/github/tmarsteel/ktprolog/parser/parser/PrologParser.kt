package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorDefinition
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.SimpleLibrary
import com.github.tmarsteel.ktprolog.parser.*
import com.github.tmarsteel.ktprolog.parser.sequence.TransactionalSequence
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.*
import com.github.tmarsteel.ktprolog.parser.lexer.*
import com.github.tmarsteel.ktprolog.parser.lexer.TokenType.*
import com.github.tmarsteel.ktprolog.parser.lexer.Operator.*
import com.github.tmarsteel.ktprolog.term.Term

class PrologParser {
    fun parseTerm(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Term> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("term")))

        tokens.mark()
        val first = tokens.next()

        if (!tokens.hasNext()) {
            // single token term
            tokens.rollback()
            return parseAtomicOrVariable(tokens)
        }

        val prefixOpsForFirst = opRegistry.findPrefixOperatorsFor(first)
    }

    fun parseAtomicOrVariable(tokens: TransactionalSequence<Token>): ParseResult<ParsedTerm> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))

        tokens.mark()

        var token = tokens.next()

        if (token is IdentifierToken) {
            tokens.commit()

            if (token.textContent[0].isUpperCase() || token.textContent[0] == '_') {
                return ParseResult(
                        ParsedVariable(token.textContent, token.location),
                        MATCHED,
                        emptySet()
                )
            }
            else
            {
                return ParseResult(
                        ParsedAtom(token.textContent, token.location),
                        MATCHED,
                        emptySet()
                )
            }
        }
        else if(token is NumericLiteralToken) {
            tokens.commit()

            val tokenNumber = token.number

            val number = when(tokenNumber) {
                is Int -> ParsedInteger(tokenNumber.toLong(), token.location)
                is Long -> ParsedInteger(tokenNumber, token.location)
                is Float -> ParsedDecimal(tokenNumber.toDouble(), token.location)
                is Double -> ParsedDecimal(tokenNumber, token.location)
                else -> throw InternalParserError("Unsupported number type in numeric literal token")
            }

            return ParseResult(
                    number,
                    MATCHED,
                    emptySet()
            )
        }
        else {
            tokens.rollback()

            return ParseResult(
                    null,
                    NOT_RECOGNIZED,
                    setOf(UnexpectedTokenError(token, IDENTIFIER, NUMERIC_LITERAL))
            )
        }
    }

    fun parseLibrary(tokens: TransactionalSequence<Token>): ParseResult<Library> {
        val reportings = mutableSetOf<Reporting>()
        val library = SimpleLibrary()

        val parsers: List<(TransactionalSequence<Token>) -> ParseResult<*>> = listOf(
            this::parseRule,
            this::parsePredicate,
            this::parsePredicateWithInfixNotation
        )

        while (tokens.hasNext()) {
            var chosenResult: ParseResult<*>? = null

            for (parser in parsers) {
                val parserResult = parser(tokens)
                if (parserResult.certainty >= MATCHED) {
                    chosenResult = parserResult
                    break
                }
            }

            if (chosenResult != null) {
                if (chosenResult.item is ParsedPredicate) {
                    library.add(chosenResult.item as ParsedPredicate)
                }
                else if (chosenResult.item is ParsedRule) {
                    library.add(chosenResult.item as ParsedRule)
                }
                else {
                    throw InternalParserError("Unsupported result: ${chosenResult.item}")
                }

                if (tokens.hasNext()) {
                    tokens.mark()
                    val fullStopToken = tokens.next()
                    if (fullStopToken is OperatorToken && fullStopToken.operator == FULL_STOP) {
                        tokens.commit()
                    }
                    else {
                        reportings += UnexpectedTokenError(fullStopToken, FULL_STOP)
                        tokens.rollback()
                    }
                }
                else {
                    reportings += UnexpectedEOFError(FULL_STOP)
                }
            }
            else {
                // none matched

                if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))
                tokens.mark()
                val token = tokens.next()
                tokens.rollback()

                return ParseResult(
                    null,
                    NOT_RECOGNIZED,
                    setOf(UnexpectedTokenError(token, "rule", "predicate"))
                )
            }
        }

        return ParseResult(
            library,
            MATCHED,
            reportings
        )
    }
}

/**
 * Skips (`next()`s) tokens in the receiver sequence until the parenthesis + bracket levels are 0 and the given
 * predicate returns false.
 */
private fun TransactionalSequence<Token>.takeWhile(predicate: (Token) -> Boolean, initialParenthesisLevel: Int = 0, initialBracketLevel: Int = 0) {
    if (!hasNext()) return

    var parenthesisLevel = initialParenthesisLevel
    var bracketLevel = initialBracketLevel

    var item: Token
    mark()

    do {
        commit()
        mark()
        item = next()

        if (item is OperatorToken) {
            when (item.operator) {
                PARENT_OPEN   -> parenthesisLevel++
                PARENT_CLOSE  -> parenthesisLevel--
                BRACKET_OPEN  -> bracketLevel++
                BRACKET_CLOSE -> bracketLevel--
            }
        }

    } while ((parenthesisLevel > 0 || bracketLevel > 0 || predicate(item)) && hasNext())

    rollback()
}

private val Token.textContent: String?
    get() = when(this) {
        is IdentifierToken -> textContent
        is OperatorToken -> operator.text
        else -> null
    }

private fun OperatorRegistry.findPrefixOperatorsFor(token: Token): Set<OperatorDefinition> {
    val tokenText = token.textContent ?: return emptySet()

    return findPrefixOperators(tokenText)
}