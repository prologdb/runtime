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
import com.github.tmarsteel.ktprolog.parser.source.SourceLocationRange
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term

/** If kotlin had union types this would be `Token | Term` */
private typealias TokenOrTerm = Any

class PrologParser {
    /**
     * @param tokens The tokens to parse from
     * @param opRegistry Is used to determine operators, their precedence and associativity
     * @param shouldStop Is invoked with the given token sequence. If it returns true the matching will stop.
     */
    fun parseTerm(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry, shouldStop: (TransactionalSequence<Token>) -> Boolean): ParseResult<Term> {
        // in prolog, the binary expression (e.g. a op b op c op d) is the concept that can be applied to all
        // syntactically valid expressions; comma separated lists are also binary expressions:
        //
        //     a, b, c, d = ','(a,','(b,','(c, d)))
        //
        // thus, this is what is parsed here

        // will hold all tokens/terms of the binary expression in sequence
        val collectedElements = ArrayList<TokenOrTerm>(10)
        val reportings = mutableSetOf<Reporting>()

        tokens.mark()

        while (!shouldStop(tokens)) {
            if (!tokens.hasNext()) {
                tokens.rollback()
                return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("term")))
            }

            val token = tokens.next()
            val parseResult = parseSingle(tokens, opRegistry)
            if (parseResult.isSuccess) {
                collectedElements.add(parseResult.item!!)
                reportings.addAll(parseResult.reportings)
            }
            else
            {
                collectedElements.add(token)
            }
        }

        if (collectedElements.isEmpty()) {
            tokens.rollback()
            throw InternalParserError()
        }

        if (collectedElements.size == 1) {
            if (collectedElements[0] is ParsedTerm) {
                tokens.commit()
                return ParseResult(
                    collectedElements[0] as ParsedTerm,
                    MATCHED,
                    emptySet()
                )
            }
            else if (collectedElements[0] is Token) {
                tokens.rollback()
                return ParseResult(
                    null,
                     NOT_RECOGNIZED,
                     setOf(UnexpectedTokenError(collectedElements[0] as Token, "term"))
                )
            }
            else throw InternalParserError()
        }

        tokens.commit()

        return buildBinaryExpressionAST(collectedElements, opRegistry)
    }

    /**
     * Parses anything that is **not** a binary expression:
     * * [parseAtomicOrVariable]
     * * [parsePredicateWithInvocationSyntax]
     * * [parseList]
     * * [parseParenthesised]
     */
    fun parseSingle(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Term> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("atom, variable, predicate invocation, list or parenthesised term")))

        val parsers = listOf<(TransactionalSequence<Token>, OperatorRegistry) -> ParseResult<Term>>(
            this::parseParenthesised,
            this::parseList,
            this::parsePredicateWithInvocationSyntax,
            { ts, _ -> parseAtomicOrVariable(ts) }
        )

        var result: ParseResult<Term>? = null
        for (parser in parsers) {
            result = parser(tokens, opRegistry)
            if (result.isSuccess) break
        }

        if (result == null || !result.isSuccess) {
            tokens.mark()
            val nextToken = tokens.next()
            tokens.rollback()

            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(nextToken, "atom, variable, predicate invocation, list or parenthesised term")))
        }

        return result
    }

    fun parsePredicateWithInvocationSyntax(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Predicate> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("predicate invocation")))

        tokens.mark()

        val nameToken = tokens.next()

        if (nameToken !is IdentifierToken && nameToken !is OperatorToken) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(nameToken, "identifier")))
        }

        if (!tokens.hasNext()) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(PARENT_OPEN)))
        }

        tokens.mark()
        val parentOpenToken = tokens.next()
        tokens.rollback()

        if (parentOpenToken !is OperatorToken || parentOpenToken.operator != PARENT_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(parentOpenToken, PARENT_OPEN)))
        }

        // the sequence <Any Token> <Parent Open> in prolog is only considered an invocation if there is no whitespace
        // between the predicate name and the opening parenthesis
        if (nameToken.location.line != parentOpenToken.location.line || nameToken.location.end.column + 1 != parentOpenToken.location.start.column) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(SyntaxError("Whitespace between predicate name and opening parenthesis not allowed", nameToken.location.end .. parentOpenToken.location.start)))
        }

        val predicateName = nameToken.textContent!!

        val argsTermResult = parseParenthesised(tokens, opRegistry)
        if (argsTermResult.isSuccess) {
            tokens.commit()
            val argsResult = commaPredicateToList(argsTermResult.item!!)
            return ParseResult(
                Predicate(predicateName, argsResult.item!!.toTypedArray()),
                    MATCHED,
                    argsTermResult.reportings + argsResult.reportings
            )
        }
        else {
            // it was ensured that the first token parseParenthesised encountered was a PARENT_OPEN
            // it is very weird that parseParenthesised fails under that condition
            throw InternalParserError()
        }
    }

    fun parseList(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<com.github.tmarsteel.ktprolog.term.List> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("list")))

        tokens.mark()

        var token = tokens.next()
        if (token !is OperatorToken || token.operator != BRACKET_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(token, BRACKET_OPEN)))
        }

        val termResult = parseTerm(tokens, opRegistry)
        val tokensUntilBracketClose = tokens.takeWhile({ it !is OperatorToken || it.operator != BRACKET_CLOSE }, 1, 0)
        tokens.commit()

        val reportings = termResult.reportings.toMutableSet()

        if (tokens.hasNext()) {
            tokens.mark()
            token = tokens.next()
            if (token is OperatorToken && token.operator == BRACKET_CLOSE) {
                tokens.commit()
            }
            else {
                tokens.rollback()
                reportings.add(UnexpectedTokenError(token, BRACKET_CLOSE))
            }
        } else {
            reportings.add(UnexpectedEOFError(BRACKET_CLOSE))
        }

        if (tokensUntilBracketClose.isNotEmpty()) {
            reportings.add(UnexpectedTokenError(tokensUntilBracketClose.first(), BRACKET_CLOSE))
        }

        val resultList: com.github.tmarsteel.ktprolog.term.List?

        if (termResult.item == null) {
            resultList = null
        } else {
            val term = termResult.item

            if (term is Predicate && term.name == HEAD_TAIL_SEPARATOR.text) {
                if (term.arity != 2) throw InternalParserError("Cannot use an instance of ${term.name}/${term.arity} to construct a list.")
                val elResult = commaPredicateToList(term.arguments[0])
                resultList = com.github.tmarsteel.ktprolog.term.List(elResult.item ?: emptyList(), term.arguments[1])
                reportings += elResult.reportings
            }
            else if (term is Predicate && term.name == COMMA.text) {
                val elResult = commaPredicateToList(term)
                resultList = com.github.tmarsteel.ktprolog.term.List(elResult.item ?: emptyList())
                reportings += elResult.reportings
            }
            else {
                resultList = com.github.tmarsteel.ktprolog.term.List(listOf(term))
            }
        }

        return ParseResult(
            resultList,
            MATCHED,
            reportings
        )
    }

    /**
     * Parses a parenthesised term: `(term)`.
     */
    fun parseParenthesised(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Term> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("parenthesised term")))

        tokens.mark()

        var token = tokens.next()
        if (token !is OperatorToken || token.operator != PARENT_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(token, PARENT_OPEN)))
        }

        val termResult = parseTerm(tokens, opRegistry)
        val tokensUntilParentClose = tokens.takeWhile({ it !is OperatorToken || it.operator != PARENT_CLOSE }, 1, 0)
        tokens.commit()

        val reportings = termResult.reportings.toMutableSet()

        if (tokens.hasNext()) {
            tokens.mark()
            token = tokens.next()
            if (token is OperatorToken && token.operator == PARENT_CLOSE) {
                tokens.commit()
            }
            else {
                tokens.rollback()
                reportings.add(UnexpectedTokenError(token, PARENT_CLOSE))
            }
        } else {
            reportings.add(UnexpectedEOFError(PARENT_CLOSE))
        }

        if (tokensUntilParentClose.isNotEmpty()) {
            reportings.add(UnexpectedTokenError(tokensUntilParentClose.first(), PARENT_CLOSE))
        }

        return ParseResult(
            termResult.item,
            MATCHED,
            reportings
        )
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

    /**
     * A comma separated list of the terms `a`, `b` and `c` (given as `a, b, c`) is parsed as
     * `','(a, ','(b, c))`. This function turns such predicates into kotlin lists of the actual
     * elements. The resulting [ParseResult] always has the certainty [MATCHED] and contains
     * [Reporting]s for every error encountered.
     */
    private fun commaPredicateToList(commaPredicate: Term): ParseResult<List<Term>> {
        TODO()
    }

    companion object {
        /**
         * Helper function for the `shouldStop` parameter to [parseTerm].
         * @return Aborts matching if the next token in the sequence is an [OperatorToken] with the given [Operator], otherwise false.
         *         Consumes the token if it matches.
         */
        fun stopAtOperator(operator: Operator): (TransactionalSequence<Token>) -> Boolean {
            return { tokens ->
                tokens.mark()
                val token = tokens.next()

                if (token is OperatorToken && token.operator == operator) {
                    tokens.commit()
                    true
                } else {
                    tokens.rollback()
                    false
                }
            }
        }

        /**
         * Helper for the `shouldStop` parameter to [parseTerm].
         * @return Aborts matching if EOF is reached.
         */
        val STOP_AT_EOF: (TransactionalSequence<Token>) -> Boolean = { !it.hasNext() }
    }
}

/**
 * Skips (`next()`s) tokens in the receiver sequence until the parenthesis + bracket levels are 0 and the given
 * predicate returns false.
 * @return The skipped tokens
 */
private fun TransactionalSequence<Token>.takeWhile(predicate: (Token) -> Boolean, initialParenthesisLevel: Int = 0, initialBracketLevel: Int = 0): List<Token> {
    if (!hasNext()) return emptyList()

    var parenthesisLevel = initialParenthesisLevel
    var bracketLevel = initialBracketLevel

    var item: Token
    mark()

    // holds the loop break condition to avoid duplicating it
    var cont: Boolean
    val tokens = ArrayList<Token>(5)

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

        cont = (parenthesisLevel > 0 || bracketLevel > 0 || predicate(item)) && hasNext()
        if (cont) {
            tokens.add(item)
        }

    } while (cont)

    rollback()

    return tokens
}

private val Token.textContent: String?
    get() = when(this) {
        is IdentifierToken -> textContent
        is OperatorToken -> operator.text
        else -> null
    }

private fun OperatorRegistry.getPrefixDefinitionOf(tokenOrTerm: TokenOrTerm): OperatorDefinition? {
    val text = tokenOrTerm.textContent ?: return null

    return getPrefixDefinition(text)
}

private val TokenOrTerm.textContent: String
    get() = when(this) {
        is Token -> this.textContent
        is Atom -> this.name
        else -> throw InternalParserError()
    }

private val TokenOrTerm.location: SourceLocationRange
    get() = when(this) {
        is Token -> location
        is ParsedTerm -> location
        else -> throw InternalParserError()
    }

private fun TokenOrTerm.asTerm(): Term {
    if (this is Term) {
        return this
    }

    if (this is Token && this is OperatorToken) {
        val text = this.textContent ?: throw InternalParserError()
        return ParsedAtom(text, this.location)
    }

    throw InternalParserError()
}

/**
 * @return The parsed predicate and the [OperatorDefinition] of its operator
 */
private fun buildBinaryExpressionAST(elements: List<TokenOrTerm>, opRegistry: OperatorRegistry): Pair<ParsedPredicate,OperatorDefinition> {
    val first = elements[0]

    val prefixDef = opRegistry.getPrefixDefinitionOf(first)

    if (prefixDef != null) {
        // prefix operator

        if (elements.size == 2) {
            val argTerm = elements[1].asTerm()

            return Pair(
                ParsedPredicate(
                    first.textContent,
                    arrayOf(argTerm),
                    first.location .. argTerm.location
                ),
                prefixDef
            )
        }

        val rhs = buildBinaryExpressionAST(elements.subList(1, elements.size), opRegistry)
        if (prefixDef.precedence > rhs.second.precedence) {
            return Pair(
                ParsedPredicate(
                    first.textContent,
                    arrayOf(rhs.first),
                    first.location .. rhs.first.location
                ),
                prefixDef
            )
        }
        else if (prefixDef.precedence < rhs.second.precedence) {
            val rhsLhs = rhs.first.arguments[0]
        }
    } else {
        // first is a value to the following infix or postfix operator
        TODO()
    }
}