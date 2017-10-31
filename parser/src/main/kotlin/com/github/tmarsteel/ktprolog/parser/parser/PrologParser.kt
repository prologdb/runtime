package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.parser.*
import com.github.tmarsteel.ktprolog.parser.sequence.TransactionalSequence
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.*
import com.github.tmarsteel.ktprolog.parser.lexer.*
import com.github.tmarsteel.ktprolog.parser.lexer.TokenType.*
import com.github.tmarsteel.ktprolog.parser.lexer.Operator.*

class PrologParser {
    fun parseQuery(tokens: TransactionalSequence<Token>): ParseResult<ParsedQuery> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))

        val firstElementResult = parseQueryElement(tokens)
        if (!firstElementResult.isSuccess) {
            return firstElementResult
        }

        val elementsAndOperators = mutableListOf<Any>()
        elementsAndOperators.add(firstElementResult.item!!)
        var reportings = mutableSetOf<Reporting>()
        reportings.addAll(firstElementResult.reportings)

        while (tokens.hasNext()) {
            tokens.mark()

            val operatorToken = tokens.next()
            if (operatorToken !is OperatorToken || (operatorToken.operator != COMMA && operatorToken.operator != SEMICOLON)) {
                tokens.rollback()
                reportings.add(UnexpectedTokenError(operatorToken, COMMA, SEMICOLON))
                break
            }

            elementsAndOperators.add(operatorToken)

            if (!tokens.hasNext()) {
                tokens.rollback()
                reportings.add(UnexpectedEOFError("query element"))
                break
            }

            val queryElementResult = parseQueryElement(tokens)
            tokens.commit()
            reportings.addAll(queryElementResult.reportings)
            if (queryElementResult.isSuccess) {
                elementsAndOperators.add(queryElementResult.item!!)
            }
        }

        return ParseResult(
            toASTWithOperatorPrecedence(elementsAndOperators),
            MATCHED,
            reportings
        )
    }

    private fun toASTWithOperatorPrecedence(queriesAndOperators: List<Any>): ParsedQuery {
        if (queriesAndOperators.size < 1) throw InternalParserError("The argument must have at least 1 item")
        if (queriesAndOperators.size == 1) return queriesAndOperators[0] as? ParsedQuery ?: throw InternalParserError("List must contain a valid boolean expression")

        val operatorsWithIndex: Collection<Pair<OperatorToken, Int>> = queriesAndOperators
            .mapIndexed { index, elem -> elem to index }
            .filter { it.first is OperatorToken } as Collection<Pair<OperatorToken, Int>>

        val leftmostOperatorWithHighestPrecedence = operatorsWithIndex
            .sortedWith(compareBy(
                { it.first.precedence },
                { it.second }
            ))
            .first()
        val operator = leftmostOperatorWithHighestPrecedence.first.operator

        // now find the last operator in that same group
        val lastOperatorWithIndexInQuery = operatorsWithIndex
            .sortedBy { it.second }
            .filter { it.second >= leftmostOperatorWithHighestPrecedence.second }
            .takeWhile { it.first.operator == leftmostOperatorWithHighestPrecedence.first.operator }
            .last()

        val indicesRelevantForThisQuery = (leftmostOperatorWithHighestPrecedence.second - 1) .. (lastOperatorWithIndexInQuery.second + 1)

        val subQueriesInQuery = queriesAndOperators
            .filterIndexed { index, _ ->  index in indicesRelevantForThisQuery }
            .filter { it is ParsedQuery } as List<ParsedQuery>

        val queryLocation = subQueriesInQuery.first().location .. subQueriesInQuery.last().location
        val query = when(operator) {
            COMMA     -> ParsedAndQuery(subQueriesInQuery.toTypedArray(), queryLocation)
            SEMICOLON -> ParsedOrQuery(subQueriesInQuery.toTypedArray(), queryLocation)
            else      -> throw InternalParserError("Unsupported boolean operator $operator")
        }

        if (indicesRelevantForThisQuery.first == 0 && indicesRelevantForThisQuery.endInclusive == queriesAndOperators.lastIndex) {
            return query
        }
        else {
            val preceding = if (indicesRelevantForThisQuery.first == 0) {
                emptyList()
            } else {
                queriesAndOperators.subList(0, indicesRelevantForThisQuery.first - 1)
            }
            val following = if (indicesRelevantForThisQuery.endInclusive == queriesAndOperators.lastIndex) {
                emptyList()
            } else {
                queriesAndOperators.subList(indicesRelevantForThisQuery.endInclusive + 1, queriesAndOperators.size)
            }

            return toASTWithOperatorPrecedence(preceding + query + following)
        }
    }

    fun parseQueryElement(tokens: TransactionalSequence<Token>): ParseResult<ParsedQuery> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))

        tokens.mark()

        var token = tokens.next()
        if (token is OperatorToken && token.operator == PARENT_OPEN) {
            // sub-query
            val subQueryResult = parseQuery(tokens)
            val reportings = mutableSetOf<Reporting>()

            token = tokens.next()
            if (token is OperatorToken && token.operator == PARENT_CLOSE) {
                reportings.add(UnexpectedTokenError(token, PARENT_CLOSE))
                tokens.takeWhile({ it is OperatorToken && it.operator != PARENT_CLOSE }, 1, 0)
            }

            tokens.commit()
            return if (reportings.isEmpty()) {
                subQueryResult
            } else {
                ParseResult(subQueryResult.item, subQueryResult.certainty, subQueryResult.reportings + reportings)
            }
        }
        else
        {
            tokens.rollback()

            val predicateResult = parsePredicate(tokens)
            val predicateQuery = if (predicateResult.item != null) {
                ParsedPredicateQuery(predicateResult.item)
            } else {
                null
            }

            return ParseResult(
                predicateQuery,
                predicateResult.certainty,
                predicateResult.reportings
            )
        }
    }

    fun parseTerm(tokens: TransactionalSequence<Token>): ParseResult<ParsedTerm> {
        val parsers: List<(TransactionalSequence<Token>) -> ParseResult<ParsedTerm>> = listOf(
                this::parsePredicate,
                this::parseAtomOrVariable,
                this::parseList
        )

        for (parser in parsers) {
            val parserResult = parser(tokens)
            if (parserResult.certainty >= MATCHED) {
                return parserResult
            }
        }

        // none matched

        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))
        tokens.mark()
        val token = tokens.next()
        tokens.rollback()

        return ParseResult(
                null,
                NOT_RECOGNIZED,
                setOf(UnexpectedTokenError(token, "atom", "variable", "list", "predicate"))
        )
    }

    fun parseList(tokens: TransactionalSequence<Token>): ParseResult<ParsedList> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))

        tokens.mark()

        val firstToken = tokens.next()

        if (firstToken is OperatorToken && firstToken.operator == BRACKET_OPEN) {
            val openingBracketToken = firstToken
            var lastLocation = openingBracketToken.location

            val itemResults = parseCommaSeparatedTerms(tokens, { it is OperatorToken && (it.operator == HEAD_TAIL_SEPARATOR || it.operator == BRACKET_CLOSE) })
            val tail: ParsedTerm?

            val reportings = itemResults.reportings.toMutableSet()

            if (itemResults.isSuccess) {
                lastLocation = itemResults.item!![itemResults.item!!.lastIndex].location

                if (tokens.hasNext()) {
                    var closingBracketOrSeparatorToken = tokens.next() as? OperatorToken ?: throw InternalParserError("Expected operator token")

                    if (closingBracketOrSeparatorToken.operator == HEAD_TAIL_SEPARATOR) {
                        val tailResult = parseTerm(tokens)
                        reportings += tailResult.reportings

                        if (tailResult.item != null) {
                            if (tailResult.item is ParsedVariable || tailResult.item is ParsedList) {
                                tail = tailResult.item
                            }
                            else {
                                tail = null
                                reportings += SemanticError("List tails must be lists or variables, ${tailResult.item} given")
                            }
                        }
                        else {
                            tail = null
                        }

                        if (tokens.hasNext()) {
                            tokens.mark()
                            val nextToken = tokens.next()
                            if (nextToken is OperatorToken && nextToken.operator == BRACKET_CLOSE) {
                                tokens.commit()
                                closingBracketOrSeparatorToken = nextToken
                            } else {
                                reportings += UnexpectedTokenError(nextToken, BRACKET_CLOSE)
                                tokens.rollback()
                            }
                        }
                    } else {
                        tail = null
                    }

                    lastLocation = closingBracketOrSeparatorToken.location

                    if (closingBracketOrSeparatorToken.operator != BRACKET_CLOSE) {
                        reportings += UnexpectedTokenError(closingBracketOrSeparatorToken, BRACKET_CLOSE)
                        tokens.takeWhile({ it !is OperatorToken || it.operator != BRACKET_CLOSE })

                        if (tokens.hasNext()) {
                            // skip closing bracket
                            lastLocation = tokens.next().location
                        } else {
                            reportings += UnexpectedEOFError(BRACKET_CLOSE)
                        }
                    }
                }
                else {
                    tail = null
                    reportings += UnexpectedEOFError(BRACKET_CLOSE)
                }

                tokens.commit()

                val location = openingBracketToken.location..lastLocation
                return ParseResult(
                        ParsedList(
                                itemResults.item,
                                tail,
                                location
                        ),
                        MATCHED,
                        reportings
                )
            } else TODO("..")
        }
        else {
            tokens.rollback()

            return ParseResult(
                    null,
                    NOT_RECOGNIZED,
                    setOf(UnexpectedTokenError(firstToken, BRACKET_OPEN))
            )
        }
    }

    fun parseAtomOrVariable(tokens: TransactionalSequence<Token>): ParseResult<ParsedTerm> {
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

            return ParseResult(
                    ParsedAtom(token.number.toString(), token.location),
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

    fun parsePredicate(tokens: TransactionalSequence<Token>): ParseResult<ParsedPredicate> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))

        tokens.mark()

        var token = tokens.next()

        if (token !is IdentifierToken) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(token, IDENTIFIER)))
        }

        val identifierToken = token

        if (!tokens.hasNext()) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(PARENT_OPEN)))
        }
        token = tokens.next()

        if (token !is OperatorToken || token.operator != Operator.PARENT_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(token, PARENT_OPEN)))
        }

        val argumentResults = parseCommaSeparatedTerms(tokens, { it is OperatorToken && it.operator == PARENT_CLOSE })

        if (argumentResults.reportings.isEmpty()) {
            // all fine
            val closingParenthesisToken = tokens.next()
            tokens.commit()

            return ParseResult(
                    ParsedPredicate(
                            identifierToken.textContent,
                            argumentResults.item!!.toTypedArray(),
                            identifierToken.location..closingParenthesisToken.location
                    ),
                    MATCHED,
                    emptySet()
            )
        }
        else
        {
            if (argumentResults.certainty < MATCHED || argumentResults.reportings.isNotEmpty()) {
                // heavy syntax error in arguments
                // but the opening parenthesis is there so this is a match
                // => skip to the next matching closing parenthesis
                tokens.takeWhile({ it !is OperatorToken || it.operator != PARENT_CLOSE }, 1, 0)
            }

            val reportings = argumentResults.reportings.toMutableSet()

            val closingParenthesisToken: Token?

            if (tokens.hasNext()) {
                tokens.mark()
                closingParenthesisToken = tokens.next()

                if (closingParenthesisToken is OperatorToken && closingParenthesisToken.operator == PARENT_CLOSE) {
                    tokens.commit()
                }
                else {
                    tokens.rollback()
                    reportings += UnexpectedTokenError(closingParenthesisToken, PARENT_CLOSE)
                }
            }
            else {
                closingParenthesisToken = null
            }

            tokens.commit()

            val location = if (closingParenthesisToken != null) identifierToken.location..(closingParenthesisToken.location) else identifierToken.location
            val arguments = if (argumentResults.item != null) argumentResults.item.toTypedArray() else emptyArray()

            return ParseResult(
                    ParsedPredicate(identifierToken.textContent, arguments, location),
                    MATCHED,
                    argumentResults.reportings
            )
        }
    }

    /**
     * Parses a comma separated list of terms. Does not report an error if the list of terms is concluded by a token
     * that matches the given predicate.
     */
    fun parseCommaSeparatedTerms(tokens: TransactionalSequence<Token>, allowedEndingPredicate: (Token) -> Boolean): ParseResult<List<ParsedTerm>> {
        tokens.mark()

        var unexpectedEndReporting: Reporting? = null

        val termResults = ArrayList<ParseResult<ParsedTerm>>()
        var mostRecentResult: ParseResult<ParsedTerm>
        var next: Token
        var hasTrailingComma = false

        do {
            mostRecentResult = parseTerm(tokens)
            if (mostRecentResult.isSuccess) {
                termResults.add(mostRecentResult)

                if (!tokens.hasNext()) {
                    // if trailing comma is true another, redundant reporting will be generated
                    if (!hasTrailingComma) {
                        unexpectedEndReporting = UnexpectedEOFError(COMMA)
                    }
                    break
                }

                tokens.mark()
                next = tokens.next()

                hasTrailingComma = false
                if (next !is OperatorToken || next.operator != COMMA) {
                    if (!allowedEndingPredicate(next)) {
                        unexpectedEndReporting = UnexpectedTokenError(next, COMMA)
                    }
                    tokens.rollback()
                    break
                }
                else {
                    hasTrailingComma = true
                }

                tokens.commit()
            }
        } while (mostRecentResult.isSuccess)

        var collectedReportings = termResults.flatMap { it.reportings }
        if (unexpectedEndReporting != null) {
            collectedReportings += unexpectedEndReporting
        }

        if (hasTrailingComma) {
            if (tokens.hasNext()) {
                tokens.mark()
                val trailing = tokens.next()
                tokens.rollback()
                collectedReportings += UnexpectedTokenError(trailing, "term")
            }
            else {
                collectedReportings += UnexpectedEOFError("term")
            }
        }

        tokens.commit()

        return ParseResult(
                termResults.map { it.item!! },
                if (termResults.size > 0) MATCHED else NOT_RECOGNIZED,
                collectedReportings
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