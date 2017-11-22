package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorDefinition
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorType.*
import com.github.tmarsteel.ktprolog.parser.*
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.MATCHED
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.NOT_RECOGNIZED
import com.github.tmarsteel.ktprolog.parser.lexer.*
import com.github.tmarsteel.ktprolog.parser.lexer.Operator.*
import com.github.tmarsteel.ktprolog.parser.lexer.TokenType.IDENTIFIER
import com.github.tmarsteel.ktprolog.parser.lexer.TokenType.NUMERIC_LITERAL
import com.github.tmarsteel.ktprolog.parser.sequence.TransactionalSequence
import com.github.tmarsteel.ktprolog.parser.source.SourceLocationRange
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable

/** If kotlin had union types this would be `Token | Term` */
private typealias TokenOrTerm = Any

class PrologParser {
    /**
     * @param tokens The tokens to parse from
     * @param opRegistry Is used to determine operators, their precedence and associativity
     * @param shouldStop Is invoked with the given token sequence. If it returns true the matching will stop.
     */
    fun parseTerm(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry, shouldStop: (TransactionalSequence<Token>) -> Boolean): ParseResult<ParsedTerm> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("term")))

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

            val parseResult = parseSingle(tokens, opRegistry)
            if (parseResult.isSuccess) {
                collectedElements.add(parseResult.item!!)
                reportings.addAll(parseResult.reportings)
            }
            else
            {
                collectedElements.add(tokens.next())
            }
        }

        if (collectedElements.isEmpty()) {
            tokens.rollback()
            if (!tokens.hasNext() || !shouldStop(tokens)) {
                return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("term")))
            } else {
                tokens.mark()
                val token = tokens.next()
                tokens.rollback()

                return ParseResult(null, MATCHED, setOf(UnexpectedTokenError(token, "term")))
            }
        }

        if (collectedElements.size == 1) {
            if (collectedElements[0] is ParsedTerm) {
                tokens.commit()
                return ParseResult(
                    collectedElements[0] as ParsedTerm,
                    MATCHED,
                    reportings
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

        val astResult = buildBinaryExpressionAST(collectedElements, opRegistry)
        return ParseResult(
            astResult.item?.first,
            astResult.certainty,
            reportings + astResult.reportings
        )
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
            if (result.certainty >= MATCHED) break
        }

        if (result == null || !result.isSuccess) {
            if (tokens.hasNext()) {
                tokens.mark()
                val nextToken = tokens.next()
                tokens.rollback()

                return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(nextToken, "atom, variable, predicate invocation, list or parenthesised term")))
            } else {
                return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("atom, variable, predicate invocation, list, parenthesised term")))
            }
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
        // rollback is not done here because detection of () invocation happens later

        if (parentOpenToken !is OperatorToken || parentOpenToken.operator != PARENT_OPEN) {
            tokens.rollback() // peek of PARENT_OPEN
            tokens.rollback() // mark() at start of method
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(parentOpenToken, PARENT_OPEN)))
        }

        // the sequence <Any Token> <Parent Open> in prolog is only considered an invocation if there is no whitespace
        // between the predicate name and the opening parenthesis
        if (nameToken.location.line != parentOpenToken.location.line || nameToken.location.end.column + 1 != parentOpenToken.location.start.column) {
            tokens.rollback() // peek of PARENT_OPEN
            tokens.rollback() // mark() at start of method
            return ParseResult(null, NOT_RECOGNIZED, setOf(SyntaxError("Whitespace between predicate name and opening parenthesis not allowed", nameToken.location.end .. parentOpenToken.location.start)))
        }

        val predicateName = nameToken.textContent!!

        // detect predicate/0 invocations
        if (!tokens.hasNext()) {
            tokens.rollback() // peek of PARENT_OPEN
            tokens.rollback() // mark() at start of method
            return ParseResult(
                ParsedPredicate(
                    predicateName,
                    emptyArray(),
                    nameToken.location .. parentOpenToken.location
                ),
                MATCHED,
                setOf(UnexpectedEOFError("[predicate arguments]", "closing parenthesis"))
            )
        }

        val tokenAfterParentOpen = tokens.next()
        if (tokenAfterParentOpen is OperatorToken && tokenAfterParentOpen.operator == PARENT_CLOSE) {
            tokens.commit() // peek of PARENT_OPEN
            tokens.commit() // mark() at start of method
            return ParseResult.of(
                ParsedPredicate(
                    predicateName,
                    emptyArray(),
                    nameToken.location .. tokenAfterParentOpen.location
                )
            )
        }
        tokens.rollback() // rollback to before PARENT_OPEN

        // arguments
        val argsTermResult = parseParenthesised(tokens, opRegistry)
        if (argsTermResult.isSuccess) {
            tokens.commit()
            val argsResult = commaPredicateToList(argsTermResult.item!!)
            return ParseResult(
                ParsedPredicate(predicateName, argsResult.item!!.toTypedArray(), nameToken.location .. argsResult.item.last().location),
                    MATCHED,
                    argsTermResult.reportings + argsResult.reportings
            )
        }
        else {
            tokens.rollback()
            return ParseResult(
                ParsedPredicate(
                    predicateName,
                    emptyArray(),
                    nameToken.location .. parentOpenToken.location
                ),
                NOT_RECOGNIZED,
                argsTermResult.reportings
            )
        }
    }

    fun parseList(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<com.github.tmarsteel.ktprolog.term.List> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("list")))

        tokens.mark()

        val openingBracketToken = tokens.next()
        if (openingBracketToken !is OperatorToken || openingBracketToken.operator != BRACKET_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(openingBracketToken, BRACKET_OPEN)))
        }

        // detect empty list
        if (!tokens.hasNext()) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("list content or ${BRACKET_CLOSE}")))
        }
        tokens.mark()
        var token = tokens.next()
        if (token is OperatorToken && token.operator == BRACKET_CLOSE) {
            tokens.commit()
            tokens.commit()
            return ParseResult(
                ParsedList(emptyList(), null, openingBracketToken.location .. token.location),
                MATCHED,
                emptySet()
            )
        }
        // else: list with content
        tokens.rollback()

        val elementsResult = parseTerm(tokens, opRegistry, { t -> stopAtOperator(HEAD_TAIL_SEPARATOR)(t) || stopAtOperator(BRACKET_CLOSE)(t) })
        if (!elementsResult.isSuccess) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, elementsResult.reportings)
        }

        val elements = commaPredicateToList(elementsResult.item ?: throw InternalParserError()).item ?: throw InternalParserError()

        if (!tokens.hasNext()) {
            tokens.commit()
            return ParseResult(
                ParsedList(elements, null, openingBracketToken.location..elementsResult.item.location),
                MATCHED,
                elementsResult.reportings + UnexpectedEOFError(HEAD_TAIL_SEPARATOR, BRACKET_CLOSE)
            )
        }

        tokens.mark()
        val tokenAfterElements = tokens.next()
        var tail: ParsedTerm?
        val reportings: MutableSet<Reporting> = elementsResult.reportings.toMutableSet()
        val tokenAfterList: Token?
        val listEndLocation: SourceLocationRange

        if (tokenAfterElements is OperatorToken && tokenAfterElements.operator == HEAD_TAIL_SEPARATOR) {
            tokens.commit()
            tokens.mark()
            val tailResult = parseTerm(tokens, opRegistry, stopAtOperator(BRACKET_CLOSE))
            tail = tailResult.item
            reportings += tailResult.reportings

            tokenAfterList = if (tokens.hasNext()) tokens.next() else null
            listEndLocation = tokenAfterList?.location ?: tokenAfterElements.location
        } else {
            tokenAfterList = tokenAfterElements
            listEndLocation = tokenAfterList.location
            tail = null
        }

        if (tokenAfterList == null) {
            reportings += UnexpectedEOFError(BRACKET_CLOSE)
        }
        else if (tokenAfterList !is OperatorToken || tokenAfterList.operator != BRACKET_CLOSE) {
            tokens.rollback()
            reportings += UnexpectedTokenError(tokenAfterList, BRACKET_CLOSE)
        }

        tokens.commit()
        return ParseResult(
            ParsedList(elements, tail, openingBracketToken.location .. listEndLocation),
            MATCHED,
            reportings
        )
    }

    /**
     * Parses a parenthesised term: `(term)`.
     */
    fun parseParenthesised(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<ParsedTerm> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("parenthesised term")))

        tokens.mark()

        var token = tokens.next()
        if (token !is OperatorToken || token.operator != PARENT_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(token, PARENT_OPEN)))
        }

        val termResult = parseTerm(tokens, opRegistry, stopAtOperator(PARENT_CLOSE))
        val tokensUntilParentClose = tokens.takeWhile({ it !is OperatorToken || it.operator != PARENT_CLOSE }, 1, 0)

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

        if (termResult.item == null) {
            tokens.rollback()
            return ParseResult(
                null,
                NOT_RECOGNIZED,
                reportings + termResult.reportings
            )
        } else {
            tokens.commit()
            return ParseResult(
                termResult.item,
                MATCHED,
                reportings + termResult.reportings
            )
        }
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
        TODO()
        /*
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
        )*/
    }

    /**
     * A comma separated list of the terms `a`, `b` and `c` (given as `a, b, c`) is parsed as
     * `','(a, ','(b, c))`. This function turns such predicates into kotlin lists of the actual
     * elements. The resulting [ParseResult] always has the certainty [MATCHED] and contains
     * [Reporting]s for every error encountered.
     */
    private fun commaPredicateToList(commaPredicate: ParsedTerm): ParseResult<List<ParsedTerm>> {
        var pivot = commaPredicate
        val list = ArrayList<ParsedTerm>(5)
        while (pivot is Predicate && pivot.arity == 2 && pivot.name == Operator.COMMA.text) {
            pivot as? ParsedPredicate ?: throw InternalParserError()
            list.add(pivot.arguments[0])
            pivot = pivot.arguments[1]
        }

        list.add(pivot)

        return ParseResult.of(list)
    }

    companion object {
        /**
         * Helper function for the `shouldStop` parameter to [parseTerm].
         * @return Aborts matching if the next token in the sequence is an [OperatorToken] with the given [Operator], otherwise false.
         *         Does not consume the final token if aborting.
         */
        fun stopAtOperator(operator: Operator): (TransactionalSequence<Token>) -> Boolean {
            return { tokens ->
                if (!tokens.hasNext()) true else {
                    tokens.mark()
                    val token = tokens.next()
                    tokens.rollback()

                    token is OperatorToken && token.operator == operator
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
    if (tokenOrTerm is Predicate || tokenOrTerm is Variable || tokenOrTerm is com.github.tmarsteel.ktprolog.term.Number) return null

    val text = tokenOrTerm.textContent

    return getPrefixDefinition(text)
}

private fun OperatorRegistry.getInfixDefinitionOf(tokenOrTerm: TokenOrTerm): OperatorDefinition? {
    if (tokenOrTerm is Predicate || tokenOrTerm is Variable || tokenOrTerm is com.github.tmarsteel.ktprolog.term.Number) return null

    val text = tokenOrTerm.textContent

    return getInfixDefinition(text)
}

private fun OperatorRegistry.getPostfixDefinitionOf(tokenOrTerm: TokenOrTerm): OperatorDefinition? {
    if (tokenOrTerm is Predicate || tokenOrTerm is Variable || tokenOrTerm is com.github.tmarsteel.ktprolog.term.Number) return null

    val text = tokenOrTerm.textContent

    return getPostfixDefinition(text)
}

private val TokenOrTerm.textContent: String
    get() = when(this) {
        is Token -> this.textContent!!
        is Atom -> this.name
        else -> throw InternalParserError()
    }

private val TokenOrTerm.location: SourceLocationRange
    get() = when(this) {
        is Token -> location
        is ParsedTerm -> location
        else -> throw InternalParserError()
    }

private fun TokenOrTerm.asTerm(): ParsedTerm {
    if (this is Term) {
        return this as? ParsedTerm ?: throw InternalParserError()
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
fun buildBinaryExpressionAST(elements: List<TokenOrTerm>, opRegistry: OperatorRegistry): ParseResult<Pair<ParsedPredicate,OperatorDefinition>> {
    if (elements.isEmpty()) throw InternalParserError()
    if (elements.size == 1) {
        TODO("A Term must be returned here; but a term does not have a meaningful OperatorDefinition. The signature of this method will have to change")
    }

    val first = elements[0]

    val prefixDef = opRegistry.getPrefixDefinitionOf(first)

    if (prefixDef != null) {
        // prefix operator

        if (elements.size == 2) {
            val argTerm = elements[1].asTerm()

            return ParseResult.of(Pair(
                ParsedPredicate(
                    first.textContent,
                    arrayOf(argTerm),
                    first.location .. argTerm.location
                ),
                prefixDef
            ))
        }
        assert(elements.size > 2)

        val entireRhsResult = buildBinaryExpressionAST(elements.subList(1, elements.size), opRegistry)
        val entireRhs = entireRhsResult.item ?: throw InternalParserError()
        if (prefixDef.precedence > entireRhs.second.precedence) {
            return ParseResult(
                Pair(
                    ParsedPredicate(
                        first.textContent,
                        arrayOf(entireRhs.first),
                        first.location .. entireRhs.first.location
                    ),
                    prefixDef
                ),
                MATCHED,
                entireRhsResult.reportings
            )
        }
        else if (prefixDef.precedence < entireRhs.second.precedence) {
            val singleRhs = elements[1].asTerm()
            val singleRhsWithPrefixOp = ParsedPredicate(
                first.textContent,
                arrayOf(singleRhs),
                first.location .. singleRhs.location
            )

            val newExpression = ArrayList<TokenOrTerm>(elements.size - 1)
            newExpression.add(singleRhsWithPrefixOp)
            newExpression.addAll(elements.subList(2, elements.size))

            return buildBinaryExpressionAST(newExpression, opRegistry)
        }
        else {
            val rhsOperatorDef = entireRhs.second
            // both same precedence; check compatibility of types
            var operatorClashError: Reporting? = null
            if (prefixDef.type == FX && (rhsOperatorDef.type == XF || rhsOperatorDef.type == XFX || rhsOperatorDef.type == XFY)) {
                operatorClashError = SemanticError("Operator priority clash: $prefixDef and $rhsOperatorDef", first.location)
            }

            if (prefixDef.type == FY && (rhsOperatorDef.type == XF || rhsOperatorDef.type == XFX || rhsOperatorDef.type == XFY)) {
                return ParseResult(
                    Pair(
                        ParsedPredicate(
                            first.textContent,
                            arrayOf(entireRhs.first),
                            first.location .. entireRhs.first.location
                        ),
                        prefixDef
                    ),
                    MATCHED,
                    entireRhsResult.reportings
                )
            }

            // the following code is technically not correct in case lhs is FX and rhs is XF or XFX or XFY
            // but still having an AST can be important. The error case is covered with a reporting earlier
            // and will prevent execution in a sane runtime.
            val singleRhs = elements[1].asTerm()
            val singleRhsWithPrefixOp = ParsedPredicate(
                    first.textContent,
                    arrayOf(singleRhs),
                    first.location .. singleRhs.location
            )

            val newExpression = ArrayList<TokenOrTerm>(elements.size - 1)
            newExpression.add(singleRhsWithPrefixOp)
            newExpression.addAll(elements.subList(2, elements.size))

            val preliminaryResult = buildBinaryExpressionAST(newExpression, opRegistry)
            // include the operatorClashReporting if necessary
            return if (operatorClashError == null) preliminaryResult else ParseResult(
                preliminaryResult.item,
                preliminaryResult.certainty,
                preliminaryResult.reportings + operatorClashError
            )
        }
    } else {
        // the first element is a parameter to the following infix or postfix op
        val second = elements[1]
        val infixDef = opRegistry.getInfixDefinitionOf(second)
        if (infixDef != null) {
            if (elements.size == 2) {
                // input: firstValue infixOp
                return ParseResult(
                    Pair(
                        ParsedPredicate(
                            second.textContent,
                            arrayOf(first.asTerm()),
                            first.location .. second.location
                        ),
                        infixDef
                    ),
                    MATCHED,
                    setOf(
                        SyntaxError("Unbalanced operator: missing second parameter to infix operator $infixDef", second.location)
                    )
                )
            }
            else if (elements.size == 3) {
                // input: firstValue infixOp secondValue
                return ParseResult.of(
                    Pair(
                        ParsedPredicate(
                            second.textContent,
                            arrayOf(first.asTerm(), elements[2].asTerm()),
                            first.location .. elements[2].location
                        ),
                        infixDef
                    )
                )
            }
            else {
                // input: firstValue infixOp ...other stuff...
                val entireRhsResult = buildBinaryExpressionAST(elements.subList(2, elements.size), opRegistry)
                if (entireRhsResult.item == null) return entireRhsResult
                val entireRhs = entireRhsResult.item

                // there is only one AST for all cases of priority and operator type; but some cases trigger
                // a clash error
                var operatorClashError: Reporting? = null

                if (infixDef.precedence < entireRhs.second.precedence || (infixDef.precedence == entireRhs.second.precedence && (infixDef.type == XFX || infixDef.type == YFX))) {
                    operatorClashError = SemanticError("Operator priority clash: $infixDef and ${entireRhs.second}", first.location)
                }

                return ParseResult(
                    Pair(
                        ParsedPredicate(
                            second.textContent,
                            arrayOf(first.asTerm(), entireRhs.first),
                            first.location .. entireRhs.first.location
                        ),
                        infixDef
                    ),
                    MATCHED,
                    if (operatorClashError == null) entireRhsResult.reportings else entireRhsResult.reportings + operatorClashError
                )
            }
        }
        else {
            val postfixDef = opRegistry.getInfixDefinitionOf(second)
            if (postfixDef == null) {
                // two values next to each other => error; then ignore the first value
                if (elements.size > 2) {
                    val result = buildBinaryExpressionAST(elements.subList(1, elements.size), opRegistry)
                    return ParseResult(
                        result.item,
                        result.certainty,
                        result.reportings + SyntaxError("Operator expected", second.location)
                    )
                } else {
                    return ParseResult<Pair<ParsedPredicate,OperatorDefinition>>(
                        null,
                        NOT_RECOGNIZED,
                        setOf(SyntaxError("Operator expected", second.location))
                    )
                }
            }
            else {
                val postfixPredicate = ParsedPredicate(
                    second.textContent,
                    arrayOf(first.asTerm()),
                    first.location .. second.location
                )

                if (elements.size == 2 || elements.size == 3) {
                    // must be value postOp [postOp]
                    if (elements.size == 2) {
                        return ParseResult.of(Pair(postfixPredicate, postfixDef))
                    }

                    return buildBinaryExpressionAST(listOf(postfixPredicate, elements[2]), opRegistry)
                }

                // turn e.g. a postOp infixOp b into infixOp(postOp(a), b))
                val newExpression = ArrayList<TokenOrTerm>(elements.size - 1)
                newExpression.add(postfixPredicate)
                newExpression.addAll(elements.subList(2, elements.size))
                val preliminaryResult = buildBinaryExpressionAST(newExpression, opRegistry)
                val preliminaryDef = preliminaryResult.item!!.second

                var reporting: Reporting? = null // will be non-null in case of a priority clash or a pointless expression (see below)

                if (postfixDef.precedence == preliminaryDef.precedence && (preliminaryDef.type == XF || preliminaryDef.type == XFX || preliminaryDef.type == XFY)) {
                    reporting = SemanticError("Operator priority clash: $postfixDef and $preliminaryDef", preliminaryResult.item.location)
                }

                if (postfixDef.precedence > preliminaryDef.precedence) {
                    reporting = SemanticError("Unsupported operator order: $postfixDef cannot precede $preliminaryDef", second.location)
                }

                if (postfixDef.precedence == preliminaryDef.precedence && postfixDef.type == YF && (preliminaryDef.type == XF || preliminaryDef.type == XFX || preliminaryDef.type == XFY)) {
                    reporting = SemanticError("Unsupported operator order: $postfixDef cannot precede $preliminaryDef", second.location)
                }

                return if (reporting == null) preliminaryResult else ParseResult(
                    preliminaryResult.item,
                    preliminaryResult.certainty,
                    preliminaryResult.reportings + reporting
                )
            }
        }
    }
}