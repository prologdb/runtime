package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.knowledge.library.*
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
import com.github.tmarsteel.ktprolog.query.Query
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable

/** If kotlin had union types this would be `Token | Term` */
private typealias TokenOrTerm = Any

class PrologParser {

    fun parseQuery(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<ParsedQuery> {
        return parseQuery(tokens, opRegistry, stopAtOperator(FULL_STOP))
    }

    fun parseQuery(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry, shouldStop: (TransactionalSequence<Token>) -> Boolean): ParseResult<ParsedQuery> {
        val termResult = parseTerm(tokens, opRegistry, shouldStop)

        if (termResult.item == null) return termResult as ParseResult<ParsedQuery>

        val transformResult = transformQuery(termResult.item)

        return ParseResult(
            transformResult.item,
            transformResult.certainty,
            termResult.reportings + transformResult.reportings
        )
    }

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
        return parseLibrary(tokens, { SimpleLibrary(SimpleLibraryEntryStore(), DefaultOperatorRegistry()) })
    }

    /**
     * @param libraryCreator Is invoked once before the parsing starts. All resluts will be added to the library
     *                       returned from the lambda. The very same instance will be returned in the library entry.
     */
    fun parseLibrary(tokens: TransactionalSequence<Token>, libraryCreator: () -> MutableLibrary): ParseResult<MutableLibrary> {
        val library = libraryCreator()
        val reportings = mutableSetOf<Reporting>()

        /**
         * Adds the given operator definition to the library.
         * @param definition The definition, e.g.: `:- op(400,xf,isDead)`
         */
        fun handleOperator(definition: ParsedPredicate) {
            val opPredicate = definition.arguments[0] as? ParsedPredicate ?: throw InternalParserError("IllegalArgument")
            if (opPredicate.arguments[0] !is com.github.tmarsteel.ktprolog.term.Integer) {
                reportings.add(SemanticError("operator priority must be an integer", opPredicate.arguments[0].location))
                return
            }

            val precedenceAsLong = (opPredicate.arguments[0] as com.github.tmarsteel.ktprolog.term.Integer).value
            if (precedenceAsLong < 0 || precedenceAsLong > 1200) {
                reportings.add(SemanticError("operator precedence must be between 0 and 1200 (inclusive)", opPredicate.arguments[0].location))
                return
            }
            val precedence = precedenceAsLong.toShort()

            if (opPredicate.arguments[1] !is Atom) {
                reportings.add(SemanticError("atom expected but found ${opPredicate.arguments[1]}", opPredicate.arguments[1].location))
                return
            }

            val typeAsUCString = (opPredicate.arguments[1] as Atom).name.toUpperCase()
            val operatorType = try {
                OperatorType.valueOf(typeAsUCString)
            }
            catch (ex: IllegalArgumentException) {
                reportings.add(SemanticError("${typeAsUCString.toLowerCase()} is not a known operator type", opPredicate.arguments[1].location))
                return
            }

            if (opPredicate.arguments[2] !is Atom) {
                reportings.add(SemanticError("Atom expected but got ${opPredicate.arguments[2]}", opPredicate.arguments[2].location))
                return
            }

            library.defineOperator(OperatorDefinition(precedence, operatorType, (opPredicate.arguments[2] as Atom).name))
        }

        fun handleRule(definition: ParsedPredicate) {
            val head = definition.arguments[0] as? ParsedPredicate ?: throw InternalParserError("Rule heads must be predicates")
            val queryTerm = definition.arguments[1] as? ParsedPredicate ?: throw InternalParserError("Queries must be predicates")
            val transformResult = transformQuery(queryTerm)

            if (transformResult.item != null) {
                library.add(ParsedRule(head, transformResult.item, head.location .. queryTerm.location))
            }

            reportings += transformResult.reportings
        }

        while (tokens.hasNext()) {
            val parseResult = parseTerm(tokens, library, stopAtOperator(FULL_STOP))
            reportings += parseResult.reportings

            if (parseResult.isSuccess) {
                val item = parseResult.item ?: throw InternalParserError("Result item should not be null")
                if (item is Predicate) {
                    item as? ParsedPredicate ?: throw InternalParserError("Expected ParsedPredicate, got Predicate")
                    // detect directive
                    if (item.isOperatorDefinition) {
                        handleOperator(item)
                    }
                    else if (item.isRuleDefinition) {
                        handleRule(item)
                    }
                    else {
                        library.add(item)
                    }
                } else {
                    val typeAsString = when(item) {
                        is Atom -> "atom"
                        is Number -> "number"
                        is Variable -> "variable"
                        else -> "term"
                    }
                    reportings += SemanticError("A $typeAsString is not a top level declaration, expected a predicate.", item.location)
                }
            }
            else {
                // continue at the next declaration
                tokens.takeWhile({ it !is OperatorToken || it.operator != FULL_STOP })
            }

            if (tokens.hasNext()) {
                // that next token MUST be a FULL_STOP, so just skip it and complain
                tokens.next()
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

    /**
     * Converts a term given as the second argument to `:-/2` into an instance of [Query].
     */
    private fun transformQuery(query: ParsedTerm): ParseResult<ParsedQuery> {
        if (query is ParsedPredicate) {
            if (query.arity == 2 && (query.name == Operator.COMMA.text || query.name == Operator.SEMICOLON.text)) {
                val operator = query.name
                val elements = ArrayList<ParsedQuery>(5)
                var pivot: ParsedTerm = query
                val reportings = mutableSetOf<Reporting>()

                fun addElement(element: ParsedTerm) {
                    val transformResult = transformQuery(element)
                    reportings += transformResult.reportings
                    if (transformResult.item != null) elements += transformResult.item
                }

                while (pivot is ParsedPredicate && pivot.arity == 2 && pivot.name == operator) {
                    addElement(pivot.arguments[0])
                    pivot = pivot.arguments[1]
                }
                addElement(pivot)

                return ParseResult(
                    if (operator == Operator.COMMA.text) {
                        ParsedAndQuery(
                            elements.toTypedArray(),
                            query.location
                        )
                    } else {
                        ParsedOrQuery(
                            elements.toTypedArray(),
                            query.location
                        )
                    },
                    MATCHED,
                    reportings
                )
            } else {
                return ParseResult.of(ParsedPredicateQuery(query))
            }
        } else {
            return ParseResult(null, NOT_RECOGNIZED, setOf(SemanticError("$query is not a valid query component", query.location)))
        }
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
 * Whether this has the form of an operator definition: `:-(op/3)`. Does not look at the types of the arguments
 * to the `op/3` invocation.
 */
private val ParsedPredicate.isOperatorDefinition: Boolean
    get() {
        if (name == HEAD_QUERY_SEPARATOR.text && arity == 1 && arguments[0] is Predicate) {
            val directivePredicate = arguments[0] as Predicate
            return directivePredicate.name == "op" && directivePredicate.arity == 3
        }

        return false
    }

private val ParsedPredicate.isRuleDefinition: Boolean
    get() = name == HEAD_QUERY_SEPARATOR.text && arity == 2 && arguments[0] is Predicate

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

private val TokenOrTerm.hasTextContent: Boolean
    get() = when(this) {
        is Token, is Atom -> true
        else -> false
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

private class ExpressionASTBuildingException(message: String) : InternalParserError(message)

/**
 * @return The parsed predicate and the [OperatorDefinition] of its operator
 */
fun buildBinaryExpressionAST(elements: List<TokenOrTerm>, opRegistry: OperatorRegistry): ParseResult<Pair<ParsedTerm,OperatorDefinition?>> {
    if (elements.isEmpty()) throw InternalParserError()
    if (elements.size == 1) {
        return ParseResult.of(Pair(elements[0].asTerm(), null))
    }

    val leftmostOperatorWithMostPrecedence: Pair<Int, Set<OperatorDefinition>> = elements
        .mapIndexed { index, it -> Pair(index, it) }
        .filter { it.second.hasTextContent }
        .map { (index, tokenOrTerm) -> Pair(index, opRegistry.getAllDefinitions(tokenOrTerm.textContent)) }
        .filter { it.second.isNotEmpty() }
        .maxBy { it.second.maxBy(OperatorDefinition::precedence)!!.precedence }
        ?: throw ExpressionASTBuildingException("There is no operator in the given list of elements")

    val index = leftmostOperatorWithMostPrecedence.first

    // will store results that can be constructed but are not necessarily the best fit to the input
    // instead of failing with an exception, this one might be returned as a surrogate
    var preliminaryResult: ParseResult<Pair<ParsedTerm,OperatorDefinition?>>? = null

    tryOperatorDefinitionForIndex@ for (operatorDef in leftmostOperatorWithMostPrecedence.second) {
        val reportings = mutableSetOf<Reporting>()

        if (operatorDef.type.isPrefix) {
            val rhsResult = buildBinaryExpressionAST(elements.subList(index + 1, elements.size), opRegistry)
            var thisTerm = ParsedPredicate(
                operatorDef.name,
                if (rhsResult.item != null) arrayOf(rhsResult.item.first) else emptyArray(),
                elements[index].location..elements.last().location
            )

            if (operatorDef.type == FX && rhsResult.item?.second != null) {
                val rhsOp = rhsResult.item.second!!
                if (rhsOp.type == YFX) {
                    val rhsPredicate = rhsResult.item.first as ParsedPredicate
                    thisTerm = ParsedPredicate(
                        rhsOp.name,
                        arrayOf(
                            ParsedPredicate(
                                operatorDef.name,
                                arrayOf(rhsPredicate.arguments[0]),
                                elements[index].location .. rhsPredicate.arguments[1].location
                            ),
                            rhsPredicate.arguments[1]
                        ),
                        elements[index].location .. rhsPredicate.arguments[1].location
                    )
                }
                else if (rhsOp.type == YF) {
                    val rhsPredicate = rhsResult.item.first as ParsedPredicate
                    thisTerm = ParsedPredicate(
                        rhsOp.name,
                        arrayOf(
                            ParsedPredicate(
                                operatorDef.name,
                                arrayOf(rhsPredicate.arguments[0]),
                                elements[index].location .. rhsPredicate.arguments[0].location
                            )
                        ),
                        elements[index].location .. rhsPredicate.location
                    )
                }
                else if (rhsOp.precedence >= operatorDef.precedence) {
                    reportings.add(SemanticError(
                        "Operator priority clash: right of ${operatorDef.name} must be strictly less precedence than ${operatorDef.precedence}, but found ${rhsOp.name} with precedence ${rhsOp.precedence}",
                        elements[index].location
                    ))
                }
            }

            val hasLhs = index > 0
            if (hasLhs) {
                val newElements = ArrayList<TokenOrTerm>(index + 2)
                newElements.addAll(elements.subList(0, index))
                newElements.add(thisTerm)
                try {
                    val fullResult = buildBinaryExpressionAST(newElements, opRegistry)
                    preliminaryResult = ParseResult(fullResult.item, fullResult.certainty, reportings + fullResult.reportings + rhsResult.reportings)
                } catch (ex: ExpressionASTBuildingException) {
                    // try another defintion for the same operator
                    continue@tryOperatorDefinitionForIndex
                }
            } else {
                preliminaryResult = ParseResult(Pair(thisTerm, operatorDef), MATCHED, reportings + rhsResult.reportings)
            }

            if (rhsResult.reportings.isEmpty()) {
                return preliminaryResult
            }
        } else if (operatorDef.type.isInfix) {
            val lhsResult = if (index > 0) {
                buildBinaryExpressionAST(elements.subList(0, index), opRegistry)
            } else {
                ParseResult(null, NOT_RECOGNIZED, setOf(SyntaxError("Missing left hand side operand", elements[index].location)))
            }

            val rhsResult = if (index < elements.lastIndex) {
                buildBinaryExpressionAST(elements.subList(index + 1, elements.size), opRegistry)
            } else {
                ParseResult(null, NOT_RECOGNIZED, setOf(SyntaxError("Missing right hand side operand", elements[index].location)))
            }

            var thisPredicate = ParsedPredicate(
                operatorDef.name,
                listOf(lhsResult.item?.first, rhsResult.item?.first).filterNotNull().toTypedArray(),
                elements.first().location..elements.last().location
            )

            reportings.addAll(lhsResult.reportings)
            reportings.addAll(rhsResult.reportings)

            if (rhsResult.item?.second != null) {
                val rhsPredicate = rhsResult.item.first as ParsedPredicate
                val rhsOp = rhsResult.item.second!!
                if ((operatorDef.type == XFX || operatorDef.type == YFX) && rhsOp.precedence >= operatorDef.precedence) {
                    if (rhsOp.type == YFX) {
                        thisPredicate = ParsedPredicate(
                            rhsPredicate.name,
                            arrayOf(
                                ParsedPredicate(
                                    operatorDef.name,
                                    arrayOf(lhsResult.item!!.first, rhsPredicate.arguments[0]),
                                    lhsResult.item.first.location .. rhsPredicate.arguments[0].location
                                ),
                                rhsPredicate.arguments[1]
                            ),
                            lhsResult.item.first.location .. rhsPredicate.arguments[1].location
                        )
                    }
                    else if (rhsOp.type == YF) {
                        thisPredicate = ParsedPredicate(
                            rhsPredicate.name,
                            arrayOf(
                                ParsedPredicate(
                                    operatorDef.name,
                                    arrayOf(lhsResult.item!!.first, rhsPredicate.arguments[0]),
                                    lhsResult.item.first.location .. rhsPredicate.arguments[0].location
                                )
                            ),
                            lhsResult.item.first.location .. rhsPredicate.location
                        )
                    }
                    else {
                        reportings.add(SemanticError(
                            "Operator priority clash: right of ${operatorDef.name} must be strictly less precedence than ${operatorDef.precedence}, but found ${rhsOp.name} with precedence ${rhsOp.precedence}",
                            elements[index].location
                        ))
                    }
                }
            }

            preliminaryResult = ParseResult(
                Pair(
                    thisPredicate,
                    operatorDef
                ),
                MATCHED,
                reportings
            )

            if (rhsResult.isSuccess) {
                return preliminaryResult
            } // else: try another operator definition
        } else if (operatorDef.type.isPostfix) {
            val lhsResult = buildBinaryExpressionAST(elements.subList(0, index), opRegistry)
            val thisTerm = ParsedPredicate(
                operatorDef.name,
                if (lhsResult.item != null) arrayOf(lhsResult.item.first) else emptyArray(),
                elements.first().location..elements[index].location
            )

            val hasRhs = index > 1
            if (hasRhs) {
                val newElements = ArrayList<TokenOrTerm>(index + 2)
                newElements.add(thisTerm)
                newElements.addAll(elements.subList(index + 1, elements.size))
                val fullResult = buildBinaryExpressionAST(newElements, opRegistry)
                return ParseResult(fullResult.item, fullResult.certainty, fullResult.reportings + lhsResult.reportings)
            } else {
                return ParseResult(Pair(thisTerm, operatorDef), MATCHED, lhsResult.reportings)
            }
        } else throw InternalParserError("Illegal operator definition: is neither prefix nor infix nor postfix")
    }

    if (preliminaryResult != null) {
        return preliminaryResult
    }

    // there is no way to use the operator
    throw InternalParserError("Cannot meaningfully use operator ${elements[index]}")
}