package com.github.prologdb.parser.parser

import com.github.prologdb.parser.ModuleDeclaration
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.SemanticWarning
import com.github.prologdb.parser.SyntaxError
import com.github.prologdb.parser.UnexpectedEOFError
import com.github.prologdb.parser.UnexpectedTokenError
import com.github.prologdb.parser.lexer.AtomLiteralToken
import com.github.prologdb.parser.lexer.IdentifierToken
import com.github.prologdb.parser.lexer.NumericLiteralToken
import com.github.prologdb.parser.lexer.Operator
import com.github.prologdb.parser.lexer.Operator.BRACKET_CLOSE
import com.github.prologdb.parser.lexer.Operator.BRACKET_OPEN
import com.github.prologdb.parser.lexer.Operator.CURLY_CLOSE
import com.github.prologdb.parser.lexer.Operator.CURLY_OPEN
import com.github.prologdb.parser.lexer.Operator.FULL_STOP
import com.github.prologdb.parser.lexer.Operator.HEAD_TAIL_SEPARATOR
import com.github.prologdb.parser.lexer.Operator.PARENT_CLOSE
import com.github.prologdb.parser.lexer.Operator.PARENT_OPEN
import com.github.prologdb.parser.lexer.OperatorToken
import com.github.prologdb.parser.lexer.StringLiteralToken
import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.lexer.TokenType.IDENTIFIER
import com.github.prologdb.parser.lexer.TokenType.NUMERIC_LITERAL
import com.github.prologdb.parser.lexer.TokenType.OPERATOR
import com.github.prologdb.parser.parser.ParseResultCertainty.MATCHED
import com.github.prologdb.parser.parser.ParseResultCertainty.NOT_RECOGNIZED
import com.github.prologdb.parser.sequence.TransactionalSequence
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.ArgumentError
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.AnonymousVariable
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorRegistry
import com.github.prologdb.runtime.util.OperatorType
import com.github.prologdb.runtime.util.OperatorType.FX
import com.github.prologdb.runtime.util.OperatorType.XFX
import com.github.prologdb.runtime.util.OperatorType.YF
import com.github.prologdb.runtime.util.OperatorType.YFX
import kotlin.math.max
import kotlin.math.min

/** If kotlin had union types this would be `Token | Term` */
private typealias TokenOrTerm = Any

class PrologParser {

    fun parseQuery(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Query> {
        val result = parseQuery(tokens, opRegistry, stopAtOperator(FULL_STOP))

        if (!result.isSuccess) {
            return result
        }

        tokens.mark()
        check(tokens.next().type == OPERATOR) // skips the FULL_STOP
        if (!tokens.hasNext()) {
            tokens.rollback()
            return result
        }

        val nextToken = tokens.next()
        tokens.rollback()

        return ParseResult(
            result.item,
            result.certainty,
            result.reportings + SyntaxError(
                "Unexpected $nextToken, expected end of input",
                nextToken.location
            )
        )
    }

    fun parseQuery(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry, shouldStop: (TransactionalSequence<Token>) -> Boolean): ParseResult<Query> {
        val termResult = parseTerm(tokens, opRegistry, shouldStop)

        if (termResult.item == null) return termResult as ParseResult<Query>

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
     * @param shouldStop Is invoked with the given token lazysequence. If it returns true the matching will stop.
     */
    fun parseTerm(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry, shouldStop: (TransactionalSequence<Token>) -> Boolean): ParseResult<Term> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("term")))

        // in prolog, the binary expression (e.g. a op b op c op d) is the concept that can be applied to all
        // syntactically valid expressions; comma separated lists are also binary expressions:
        //
        //     a, b, c, d = ','(a,','(b,','(c, d)))
        //
        // thus, this is what is parsed here

        // will hold all tokens/terms of the binary expression in lazysequence
        val collectedElements = ArrayList<TokenOrTerm>(10)
        val reportings = mutableSetOf<Reporting>()

        tokens.mark()

        while (tokens.hasNext() && !shouldStop(tokens)) {
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

        if (!shouldStop(tokens)) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("term")))
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
            if (collectedElements[0] is Term) {
                tokens.commit()
                return ParseResult(
                    collectedElements[0] as Term,
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

        try {
            val astResult = buildExpressionAST(collectedElements, opRegistry)

            return ParseResult(
                astResult.item?.first,
                astResult.certainty,
                reportings + astResult.reportings
            )
        } catch (ex: ExpressionASTBuildingException) {
            return ParseResult(
                collectedElements[0].asTerm(),
                MATCHED,
                setOf(ex.reporting)
            )
        }
    }

    /**
     * Parses anything that is **not** a binary expression:
     * * [parseAtomicOrVariable]
     * * [parseCompoundTerm]
     * * [parseList]
     * * [parseParenthesised]
     */
    fun parseSingle(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Term> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("atom, variable, compound term, list or parenthesised term")))

        val parsers = listOf<(TransactionalSequence<Token>, OperatorRegistry) -> ParseResult<Term>>(
            { seq, reg -> this.parseParenthesised(seq, reg).map { it.first } },
            this::parseList,
            this::parseDictionary,
            this::parseCompoundTerm,
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

                return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(nextToken, "atom, variable, compound term, list or parenthesised term")))
            } else {
                return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("atom, variable, compound term, list, parenthesised term")))
            }
        }

        return result
    }

    fun parseCompoundTerm(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<CompoundTerm> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("predicate invocation")))

        tokens.mark()

        val functorToken = tokens.next()

        if (functorToken !is IdentifierToken) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(functorToken, "identifier")))
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

        // the lazysequence <Any Token> <Parent Open> in prolog is only considered an invocation if there is no whitespace
        // between the predicate name and the opening parenthesis
        if (functorToken.location.line != parentOpenToken.location.line || functorToken.location.end.column + 1 != parentOpenToken.location.start.column) {
            tokens.rollback() // peek of PARENT_OPEN
            tokens.rollback() // mark() at start of method
            return ParseResult(null, NOT_RECOGNIZED, setOf(SyntaxError("Whitespace between functor and opening parenthesis not allowed", functorToken.location.end..parentOpenToken.location.start)))
        }

        val functor = functorToken.textContent

        // detect predicate/0 invocations
        if (!tokens.hasNext()) {
            tokens.rollback() // peek of PARENT_OPEN
            tokens.rollback() // mark() at start of method
            return ParseResult(
                CompoundTerm(
                    functor,
                    emptyArray()
                ).also { it.sourceInformation = functorToken.location..parentOpenToken.location },
                MATCHED,
                setOf(UnexpectedEOFError("[compound term arguments]", "closing parenthesis"))
            )
        }

        val tokenAfterParentOpen = tokens.next()
        if (tokenAfterParentOpen is OperatorToken && tokenAfterParentOpen.operator == PARENT_CLOSE) {
            tokens.commit() // peek of PARENT_OPEN
            tokens.commit() // mark() at start of method
            return ParseResult.of(
                CompoundTerm(
                    functor,
                    emptyArray()
                ).also {
                    it.sourceInformation = functorToken.location..tokenAfterParentOpen.location
                }
            )
        }
        tokens.rollback() // rollback to before PARENT_OPEN

        // arguments
        val argsTermResult = parseParenthesised(tokens, opRegistry, true)
        if (argsTermResult.isSuccess) {
            tokens.commit()
            val argsResult = commaCompoundToList(argsTermResult.item!!.first)
            return ParseResult(
                CompoundTerm(
                    functor,
                    argsResult.item!!.toTypedArray()
                ).also { it.sourceInformation = functorToken.location..argsTermResult.item.second },
                MATCHED,
                argsTermResult.reportings + argsResult.reportings
            )
        }
        else {
            tokens.rollback()
            return ParseResult(
                CompoundTerm(
                    functor,
                    emptyArray()
                ).also { it.sourceInformation = functorToken.location..parentOpenToken.location },
                NOT_RECOGNIZED,
                argsTermResult.reportings
            )
        }
    }

    fun parseList(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<PrologList> {
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
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("list content or $BRACKET_CLOSE")))
        }
        tokens.mark()
        val token = tokens.next()
        if (token is OperatorToken && token.operator == BRACKET_CLOSE) {
            tokens.commit()
            tokens.commit()
            return ParseResult(
                PrologList(emptyList(), null).also { it.sourceInformation = openingBracketToken.location..token.location },
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

        val elements = commaCompoundToList(elementsResult.item ?: throw InternalParserError()).item ?: throw InternalParserError()

        if (!tokens.hasNext()) {
            tokens.commit()
            return ParseResult(
                PrologList(elements, null).also { it.sourceInformation = openingBracketToken.location..elementsResult.item.location },
                MATCHED,
                elementsResult.reportings + UnexpectedEOFError(HEAD_TAIL_SEPARATOR, BRACKET_CLOSE)
            )
        }

        tokens.mark()
        val tokenAfterElements = tokens.next()
        val tail: Term?
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
            PrologList(elements, tail).also { it.sourceInformation = openingBracketToken.location..listEndLocation },
            MATCHED,
            reportings
        )
    }

    fun parseDictionary(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<PrologDictionary> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("dict")))

        tokens.mark()

        val openingCurlyToken = tokens.next()
        if (openingCurlyToken !is OperatorToken || openingCurlyToken.operator != CURLY_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(openingCurlyToken, CURLY_OPEN)))
        }

        // detect empty dict
        if (!tokens.hasNext()) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("dict content or $CURLY_CLOSE")))
        }
        tokens.mark()
        val token = tokens.next()
        if (token is OperatorToken && token.operator == CURLY_CLOSE) {
            tokens.commit()
            tokens.commit()
            return ParseResult(
                PrologDictionary(emptyMap(), null).also { it.sourceInformation = openingCurlyToken.location..token.location },
                MATCHED,
                emptySet()
            )
        }
        // else: dict with content
        tokens.rollback()

        val elementsResult = parseTerm(tokens, opRegistry, stopAtAnyOf(HEAD_TAIL_SEPARATOR, CURLY_CLOSE))
        if (!elementsResult.isSuccess) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, elementsResult.reportings)
        }

        val reportings: MutableSet<Reporting> = elementsResult.reportings.toMutableSet()

        val elements = commaCompoundToList(elementsResult.item ?: throw InternalParserError()).item ?: throw InternalParserError()

        // complain about every element that is not an instance of :/2 with the first argument being an atom
        val (elementsInstanceOfPairArity2, elementsNotInstanceOfPair) = elements.partition { it is CompoundTerm && it.functor == ":" && it.arity == 2 }
        val (validPairs, elementsWithKeyNotAnAtom) = elementsInstanceOfPairArity2.partition { (it as CompoundTerm).arguments[0] is Atom }

        elementsNotInstanceOfPair.forEach {
            reportings.add(SyntaxError("Elements in a dict literal must be instances of :/2", it.location))
        }
        elementsWithKeyNotAnAtom.forEach {
            reportings.add(SyntaxError("Keys in dict pairs must be atoms", (it as CompoundTerm).arguments[0].location))
        }

        val pairsAsKotlinPairs: List<Pair<Atom, Term>> = validPairs
            .map {
                it as CompoundTerm
                (it.arguments[0] as Atom) to it.arguments[1]
            }
        val pairsAsKotlinMap = pairsAsKotlinPairs.toMap()

        if (pairsAsKotlinMap.size < pairsAsKotlinPairs.size) {
            // there were duplicates, issue a warning each
            pairsAsKotlinPairs
                .groupBy { it.first }
                .filter { it.value.size > 1 }
                .forEach {
                    reportings.add(SemanticWarning("Duplicate key in dict: ${it.key.name}", openingCurlyToken.location..elementsResult.item.location))
                }
        }

        if (!tokens.hasNext()) {
            tokens.commit()
            return ParseResult(
                PrologDictionary(pairsAsKotlinMap, null).also { it.sourceInformation = openingCurlyToken.location..elementsResult.item.location },
                MATCHED,
                elementsResult.reportings + UnexpectedEOFError(HEAD_TAIL_SEPARATOR, CURLY_CLOSE)
            )
        }

        tokens.mark()
        val tokenAfterElements = tokens.next()
        val tail: Term?
        val tokenAfterList: Token?
        val dictEndLocation: SourceLocationRange

        if (tokenAfterElements is OperatorToken && tokenAfterElements.operator == HEAD_TAIL_SEPARATOR) {
            tokens.commit()
            tokens.mark()
            val tailResult = parseTerm(tokens, opRegistry, stopAtOperator(CURLY_CLOSE))
            tail = tailResult.item
            reportings += tailResult.reportings

            tokenAfterList = if (tokens.hasNext()) tokens.next() else null
            dictEndLocation = tokenAfterList?.location ?: tokenAfterElements.location
        } else {
            tokenAfterList = tokenAfterElements
            dictEndLocation = tokenAfterList.location
            tail = null
        }

        if (tokenAfterList == null) {
            reportings += UnexpectedEOFError(CURLY_CLOSE)
        }
        else if (tokenAfterList !is OperatorToken || tokenAfterList.operator != CURLY_CLOSE) {
            tokens.rollback()
            reportings += UnexpectedTokenError(tokenAfterList, CURLY_CLOSE)
        }

        tokens.commit()
        return ParseResult(
            PrologDictionary(pairsAsKotlinMap, tail).also { it.sourceInformation = openingCurlyToken.location..dictEndLocation },
            MATCHED,
            reportings
        )
    }

    fun parseParenthesised(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Pair<Term, SourceLocation>> {
        return parseParenthesised(tokens, opRegistry, false)
    }

    /**
     * Parses a parenthesised term: `(term)`.
     * @param outmostWithoutProtection If the term within the parenthesis is a compound, does not set the [CompoundTerm.parenthesisProtection] flag.
     * @return first: the term, second: location of the closing parenthesis
     */
    fun parseParenthesised(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry, outmostWithoutProtection: Boolean): ParseResult<Pair<Term, SourceLocation>> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("parenthesised term")))

        tokens.mark()

        var token = tokens.next()
        if (token !is OperatorToken || token.operator != PARENT_OPEN) {
            tokens.rollback()
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedTokenError(token, PARENT_OPEN)))
        }

        val termResult = parseTerm(tokens, opRegistry, stopAtOperator(PARENT_CLOSE))
        val tokensUntilParentClose = tokens.takeWhile({ it !is OperatorToken || it.operator != PARENT_CLOSE }, 1, 0)
        val locationAfterTerm: SourceLocation

        val reportings = termResult.reportings.toMutableSet()

        if (tokens.hasNext()) {
            tokens.mark()
            token = tokens.next()
            locationAfterTerm = token.location.end
            if (token is OperatorToken && token.operator == PARENT_CLOSE) {
                tokens.commit()
            }
            else {
                tokens.rollback()
                reportings.add(UnexpectedTokenError(token, PARENT_CLOSE))
            }
        } else {
            reportings.add(UnexpectedEOFError(PARENT_CLOSE))
            locationAfterTerm = termResult.location.end
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
            val item = termResult.item
            if (!outmostWithoutProtection && item is CompoundTerm) {
                item.parenthesized = true
            }

            return ParseResult(
                Pair(item, locationAfterTerm),
                MATCHED,
                reportings + termResult.reportings
            )
        }
    }

    fun parseAtomicOrVariable(tokens: TransactionalSequence<Token>): ParseResult<Term> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))

        tokens.mark()

        val token = tokens.next()

        if (token is IdentifierToken) {
            tokens.commit()

            if (token.textContent == "_") {
                return ParseResult(
                    AnonymousVariable().also { it.sourceInformation = token.location },
                    MATCHED,
                    emptySet()
                )
            }
            else if (token.textContent[0].isUpperCase() || token.textContent[0] == '_') {
                return ParseResult(
                    Variable(token.textContent).also {
                        it.sourceInformation = token.location
                    },
                    MATCHED,
                    emptySet()
                )
            }
            else
            {
                return ParseResult(
                    Atom(token.textContent).also {
                        it.sourceInformation = token.location
                    },
                    MATCHED,
                    emptySet()
                )
            }
        }
        else if(token is NumericLiteralToken) {
            tokens.commit()

            val tokenNumber = token.number

            val number = when(tokenNumber) {
                is Int -> PrologInteger(tokenNumber.toLong())
                is Long -> PrologInteger(tokenNumber)
                is Float -> PrologDecimal(tokenNumber.toDouble())
                is Double -> PrologDecimal(tokenNumber)
                else -> throw InternalParserError("Unsupported number type in numeric literal token")
            }
            number.sourceInformation = token.location

            return ParseResult(
                    number,
                    MATCHED,
                    emptySet()
            )
        }
        else if (token is StringLiteralToken) {
            tokens.commit()

            return ParseResult(
                PrologString(token.content).also {
                    it.sourceInformation = token.location
                },
                MATCHED,
                emptySet()
            )
        }
        else if (token is AtomLiteralToken) {
            tokens.commit()
            return ParseResult(
                Atom(token.name).also {
                    it.sourceInformation = token.location
                    it.quoted = token.quoted
                },
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

    /**
     * Converts the AST of a `:- op/3` directive into an [OperatorDefinition].
     *
     * Formally, the AST must succeed this goal:
     *
     *     OpDefinitionAST = op(Precedence, Associativity, Name),
     *     integer(Precedence), Precedence >= 0, Precedence =< 1200,
     *     member(Associativity, [fx, fy, xfx, xfy, yfx, xf, yf]),
     *     atom(name).
     *
     * @param opDefinitionAST The instance of `op/3`
     */
    fun parseOperatorDefinition(opDefinitionAST: CompoundTerm): ParseResult<OperatorDefinition> {
        require(opDefinitionAST.arity == 3)

        val precedenceArgument = opDefinitionAST.arguments[0]
        if (precedenceArgument !is PrologNumber || !precedenceArgument.isInteger) {
            return ParseResult(null, MATCHED, setOf(
                SemanticError("operator precedence must be an integer", precedenceArgument.sourceInformation as SourceLocation)
            ))
        }

        val reportings = mutableSetOf<Reporting>()

        var precedenceAsLong = precedenceArgument.toInteger()
        if (precedenceAsLong < 0 || precedenceAsLong > 1200) {
            reportings.add(SemanticError("operator precedence must be between 0 and 1200 (inclusive)", opDefinitionAST.arguments[0].sourceInformation as SourceLocation))
            precedenceAsLong = min(max(0, precedenceAsLong), 1200)
        }
        val precedence = precedenceAsLong.toShort()

        if (opDefinitionAST.arguments[1] !is Atom) {
            reportings.add(SemanticError("operator type: expected atom but got ${opDefinitionAST.arguments[1].prologTypeName}", opDefinitionAST.arguments[1].sourceInformation as SourceLocation))
            return ParseResult(null, MATCHED, reportings)
        }

        val typeAsUCString = (opDefinitionAST.arguments[1] as Atom).name.toUpperCase()
        val operatorType = try {
            OperatorType.valueOf(typeAsUCString)
        }
        catch (ex: IllegalArgumentException) {
            reportings.add(SemanticError("${typeAsUCString.toLowerCase()} is not a known operator type", opDefinitionAST.arguments[1].sourceInformation as SourceLocation))
            return ParseResult(null, MATCHED, reportings)
        }

        if (opDefinitionAST.arguments[2] !is Atom) {
            reportings.add(SemanticError("operator name: expected atom but got ${opDefinitionAST.arguments[2].prologTypeName}", opDefinitionAST.arguments[2].sourceInformation as SourceLocation))
            return ParseResult(null, MATCHED, reportings)
        }

        return ParseResult(
            OperatorDefinition(precedence, operatorType, (opDefinitionAST.arguments[2] as Atom).name),
            MATCHED,
            reportings
        )
    }

    /**
     * Converts the AST of a `:- module/1` or `:- module/2` directive into a [ModuleDeclaration].
     *
     * Formally, the AST must succeed this goal:
     *
     *     compound_name_arguments(ModuleDeclarationAST, module, Args),
     *     [Name|_] = Args,
     *     atom(Name),
     *     (length(Args, 1) ; length(Args, 2),
     *         [_, Exports] = Args,
     *         list(Exports),
     *         member(Export, Exports),
     *         valid_clause_indicator(Export)
     *     ).
     *
     * @param moduleDeclarationAST The instance of `module/1` or `module/2`
     */
    fun parseModuleDeclaration(moduleDeclarationAST: CompoundTerm): ParseResult<ModuleDeclaration> {
        val args = moduleDeclarationAST.arguments
        require(args.size in 1..2)

        if (args[0] !is Atom) {
            return ParseResult(null, MATCHED, setOf(SemanticError(
                "Argument 0 to module/${args.size} must be an atom, got ${args[0].prologTypeName}",
                args[0].sourceInformation as SourceLocation
            )))
        }

        val name = (args[0] as Atom).name
        var exportSelection: Set<ClauseIndicator>? = null

        val reportings = mutableSetOf<Reporting>()
        if (args.size == 2) {
            if (args[1] is PrologList) {
                exportSelection = (args[1] as PrologList).elements
                    .mapNotNull {
                        val indicatorResult = parseIdiomaticClauseIndicator(it)
                        reportings.addAll(indicatorResult.reportings)
                        indicatorResult.item
                    }
                    .toSet()
            } else {
                reportings.add(
                    SemanticError(
                        "Argument 1 to module/2 must be a list, got ${args[1].prologTypeName}",
                        args[1].sourceInformation as SourceLocation
                    )
                )
            }
        }

        return ParseResult(
            ModuleDeclaration(name, exportSelection),
            MATCHED,
            reportings
        )
    }

    /**
     * Converts the AST of a `:- use_module/1` or `:- use_module/2` directive into a [ModuleImport].
     *
     * Formally, the given AST must succeed this goal:
     *
     *     compound_name_arguments(ImportAST, use_module, Args),
     *     [Ref|_] = Args,
     *     compound_name_arguments(Ref, Group, [RefName]),
     *     atom(Group),
     *     atom(RefName),
     *     (length(Args, 1); length(Args, 2),
     *         [_, Selection] = Args,
     *         (
     *             is_list(Selection),
     *             member(SelectionE, Selection),
     *             valid_clause_indicator(SelectionE)
     *         ) ; (
     *             except(Exclusions) = Selection,
     *             is_list(Exclusions),
     *             member(ExclusionE, Exclusions),
     *             valid_clause_indicator(ExclusionE)
     *         )
     *     )
     *
     * @param importAST The instance of `use_module/1` or `use_module/2`
     */
    fun parseModuleImport(importAST: CompoundTerm) : ParseResult<ModuleImport> {
        val args = importAST.arguments
        require(args.size in 1..2)

        val moduleRefTerm = args[0]
        if (moduleRefTerm !is CompoundTerm) {
            return ParseResult(null, MATCHED, setOf(
                SemanticError("Argument 1 to use_module/${args.size} must be a compound term, got ${moduleRefTerm.prologTypeName}", importAST.sourceInformation as SourceLocation)
            ))
        }

        if (moduleRefTerm.arity != 1 || moduleRefTerm.arguments[0] !is Atom) {
            return ParseResult(null, MATCHED, setOf(
                SemanticError("Illegal module reference: must be of arity 1 and the sole argument must be an atom", importAST.sourceInformation as SourceLocation)
            ))
        }

        val moduleReference = ModuleReference(moduleRefTerm.functor, (moduleRefTerm.arguments[0] as Atom).name)

        if (args.size == 1) {
            return ParseResult.of(ModuleImport.Full(moduleReference))
        }

        val selectionTerm = args[1]

        if (selectionTerm is PrologList) {
            val imports = mutableMapOf<ClauseIndicator, String>()
            val reportings = mutableSetOf<Reporting>()
            importTerms@for (importTerm in selectionTerm.elements) {
                if (importTerm !is CompoundTerm) {
                    reportings.add(SyntaxError(
                        "References to single predicates in argument 1 to use_module/2 must be compounds, got ${importTerm.prologTypeName}",
                        importTerm.sourceInformation as SourceLocation
                    ))
                    continue@importTerms
                }

                when (importTerm.functor) {
                    "/"  -> {
                        val indicatorResult = parseIdiomaticClauseIndicator(importTerm)
                        reportings.addAll(indicatorResult.reportings)
                        indicatorResult.item?.let { imports[it] = it.functor }
                    }
                    "as" -> {
                        val indicatorResult = parseIdiomaticClauseIndicator(importTerm.arguments[0])
                        reportings.addAll(indicatorResult.reportings)
                        if (indicatorResult.item == null) {
                            continue@importTerms
                        }
                        val aliasTerm = importTerm.arguments[1]
                        if (aliasTerm is Atom) {
                            imports[indicatorResult.item] = aliasTerm.name
                        } else {
                            reportings.add(SyntaxError(
                                "Predicate aliases in argument 1 to use_module/2 must be atoms, got ${aliasTerm.prologTypeName}",
                                aliasTerm.sourceInformation as SourceLocation
                            ))
                        }
                    }
                    else -> {
                        reportings.add(SyntaxError(
                            "References to single predicates in argument 1 to use_module/2 must unify with either _/_ or _/_ as _",
                            importTerm.sourceInformation as SourceLocation
                        ))
                    }
                }
            }

            return ParseResult(
                ModuleImport.Selective(moduleReference, imports),
                MATCHED,
                reportings
            )
        } else if (selectionTerm is CompoundTerm && selectionTerm.functor == "except" && selectionTerm.arity == 1) {
            val listTerm = selectionTerm.arguments[0]
            if (listTerm !is PrologList) {
                return ParseResult(null, MATCHED, setOf(SyntaxError(
                    "Argument 1 to except/1 in argument 1 to use_module/2 must be a list, got ${listTerm.prologTypeName}",
                    listTerm.sourceInformation as SourceLocation
                )))
            }

            val reportings = mutableSetOf<Reporting>()
            val except = mutableSetOf<ClauseIndicator>()

            for (exceptTerm in listTerm.elements) {
                val indicatorResult = parseIdiomaticClauseIndicator(exceptTerm)
                reportings.addAll(indicatorResult.reportings)
                indicatorResult.item?.let { except.add(it) }
            }

            return ParseResult(
                ModuleImport.Except(moduleReference, except),
                MATCHED,
                reportings
            )
        } else {
            throw ArgumentError(
                ClauseIndicator.of("use_module", 2),
                1,
                "must be either a list or an instance of except/1, got ${selectionTerm.prologTypeName}"
            )
        }
    }

    /**
     * Converts a term that should denote a clause indicator to a [ClauseIndicator].
     *
     * Formally, the given AST must succeed this goal:
     *
     *     Term = /(Name, Arity),
     *     atom(Name),
     *     integer(Arity),
     *     Arity >= 0.
     * @param term the indicator AST
     */
    fun parseIdiomaticClauseIndicator(term: Term): ParseResult<ClauseIndicator> {
        if (term !is CompoundTerm || term.arity != 2 || term.functor != "/") {
            return ParseResult(null, MATCHED, setOf(SyntaxError(
                "Predicate indicators must be instances of `/`/2",
                term.sourceInformation as SourceLocation
            )))
        }

        val functorTerm = term.arguments[0]
        val arityTerm = term.arguments[1]

        if (functorTerm !is Atom) {
            return ParseResult(null, MATCHED, setOf(SyntaxError(
                "Predicate functors must be atoms, got ${functorTerm.prologTypeName}",
                functorTerm.sourceInformation as SourceLocation
            )))
        }

        if (arityTerm !is PrologInteger) {
            return ParseResult(null, MATCHED, setOf(SyntaxError(
                "Predicate arities must be integers, got ${functorTerm.prologTypeName}",
                arityTerm.sourceInformation as SourceLocation
            )))
        }

        if (arityTerm.toInteger() < 0) {
            return ParseResult(null, MATCHED, setOf(SyntaxError(
                "Predicate arity cannot be negative",
                arityTerm.sourceInformation as SourceLocation
            )))
        }

        if (arityTerm.toInteger() > Int.MAX_VALUE) {
            return ParseResult(null, MATCHED, setOf(SyntaxError(
                "Predicate arity cannot be negative",
                arityTerm.sourceInformation as SourceLocation
            )))
        }

        return ParseResult.of(ClauseIndicator.of(functorTerm.name, arityTerm.toInteger().toInt()))
    }

    fun <Result : Any> parseSourceFile(
        tokens: TransactionalSequence<Token>,
        visitor: SourceFileVisitor<Result>
    ): ParseResult<Result> {
        val reportings = mutableListOf<Reporting>()

        while (tokens.hasNext()) {
            val parseResult = parseTerm(tokens, visitor.operators, stopAtOperator(FULL_STOP))
            reportings += parseResult.reportings

            if (parseResult.isSuccess) {
                val item = parseResult.item ?: throw InternalParserError("Result item should not be null")
                if (item is CompoundTerm) {
                    if (item.isDirectiveInvocation) {
                        reportings += visitor.visitDirective(item.arguments[0] as CompoundTerm)
                    }
                    else if (item.isRuleDefinition) {
                        val head = item.arguments[0] as? CompoundTerm ?: throw InternalParserError("Rule heads must be compound term")
                        val queryTerm = item.arguments[1] as? CompoundTerm ?: throw InternalParserError("Queries must be compound term")
                        val transformResult = transformQuery(queryTerm)
                        reportings += transformResult.reportings

                        if (transformResult.item != null) {
                            val location = head.location..queryTerm.location
                            val rule = Rule(head, transformResult.item).apply {
                                sourceInformation = location
                            }
                            reportings += visitor.visitClause(rule, location)
                        }
                    }
                    else {
                        reportings += visitor.visitClause(item, item.location)
                    }
                } else {
                    reportings += visitor.visitNonClause(item)
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

        val result = visitor.buildResult()
        return if (reportings.isEmpty()) result else {
            ParseResult(
                result.item,
                result.certainty,
                (reportings + result.reportings).toSet()
            )
        }
    }

    /**
     * A comma separated list of the terms `a`, `b` and `c` (given as source `a, b, c`) is parsed as
     * `','(a, ','(b, c))`. This function turns such compounds into kotlin lists of the actual
     * elements. The resulting [ParseResult] always has the certainty [MATCHED] and contains
     * [Reporting]s for every error encountered.
     */
    private fun commaCompoundToList(commaCompound: Term): ParseResult<List<Term>> {
        var pivot = commaCompound
        val list = ArrayList<Term>(5)
        while (pivot is CompoundTerm && !pivot.parenthesized && pivot.arity == 2 && pivot.functor == Operator.COMMA.text) {
            pivot as? CompoundTerm ?: throw InternalParserError()
            list.add(pivot.arguments[0])
            pivot = pivot.arguments[1]
        }

        list.add(pivot)

        return ParseResult.of(list)
    }

    /**
     * Converts a term given as the second argument to `:-/2` into an instance of [Query].
     */
    fun transformQuery(query: Term): ParseResult<Query> {
        if (query is CompoundTerm) {
            if (query.arity == 2 && (query.functor == Operator.COMMA.text || query.functor == Operator.SEMICOLON.text)) {
                val operator = query.functor
                val elements = ArrayList<Query>(5)
                var pivot: Term = query
                val reportings = mutableSetOf<Reporting>()

                fun addElement(element: Term) {
                    val transformResult = transformQuery(element)
                    reportings += transformResult.reportings
                    if (transformResult.item != null) elements += transformResult.item
                }

                while (pivot is CompoundTerm && pivot.arity == 2 && pivot.functor == operator) {
                    addElement(pivot.arguments[0])
                    pivot = pivot.arguments[1]
                }
                addElement(pivot)

                return ParseResult(
                    if (operator == Operator.COMMA.text) {
                        AndQuery(elements.toTypedArray()).also { it.sourceInformation = query.location }
                    } else {
                        OrQuery(elements.toTypedArray()).also { it.sourceInformation = query.location }
                    },
                    MATCHED,
                    reportings
                )
            } else {
                return ParseResult.of(PredicateInvocationQuery(query).also {
                    it.sourceInformation = query.sourceInformation
                })
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
                if (tokens.hasNext()) {
                    tokens.mark()
                    val token = tokens.next()
                    tokens.rollback()

                    token is OperatorToken && token.operator == operator
                } else false
                // return false so that EOF can be detected independently of the break condition
            }
        }

        fun stopAtAnyOf(operator1: Operator, operator2: Operator): (TransactionalSequence<Token>) -> Boolean {
            return { tokens ->
                if (tokens.hasNext()) {
                    tokens.mark()
                    val token = tokens.next()
                    tokens.rollback()

                    token is OperatorToken && (token.operator == operator1 || token.operator == operator2)
                } else false
            }
        }

        /**
         * Helper for the `shouldStop` parameter to [parseTerm].
         * @return Aborts matching if EOF is reached.
         */
        val STOP_AT_EOF: (TransactionalSequence<Token>) -> Boolean = { !it.hasNext() }
    }
}

private val CompoundTerm.isDirectiveInvocation: Boolean
    get() = functor == Operator.HEAD_QUERY_SEPARATOR.text && arity == 1 && arguments[0] is CompoundTerm

private val CompoundTerm.isRuleDefinition: Boolean
    get() = functor == Operator.HEAD_QUERY_SEPARATOR.text && arity == 2 && arguments[0] is CompoundTerm

/**
 * Skips (`next()`s) tokens in the receiver sequence until the parenthesis + bracket levels are 0 and the given
 * predicate returns false.
 * @return The skipped tokens
 */
private fun TransactionalSequence<Token>.takeWhile(predicate: (Token) -> Boolean, initialParenthesisLevel: Int = 0, initialBracketLevel: Int = 0, initialCurlyLevel: Int = 0): List<Token> {
    if (!hasNext()) return emptyList()

    var parenthesisLevel = initialParenthesisLevel
    var bracketLevel = initialBracketLevel
    var curlyLevel = initialCurlyLevel

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
                CURLY_OPEN    -> curlyLevel++
                CURLY_CLOSE   -> curlyLevel--
                else -> {}
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
        is Term -> sourceInformation as? SourceLocationRange
            ?: throw InternalParserError()
        else -> throw InternalParserError()
    }

private fun TokenOrTerm.asTerm(): Term {
    if (this is Term) return this

    if (this is Token && this is OperatorToken) {
        val text = this.textContent ?: throw InternalParserError()
        return Atom(text).also { it.sourceInformation = this.location}
    }

    throw InternalParserError()
}

private class ExpressionASTBuildingException(val reporting: Reporting) : RuntimeException()

/**
 * @return The parsed term and the [OperatorDefinition] of its operator; if the parsed term does not involve an
 *         operator, the operator is null
 * @throws ExpressionASTBuildingException
 */
private fun buildExpressionAST(elements: List<TokenOrTerm>, opRegistry: OperatorRegistry): ParseResult<Pair<Term, OperatorDefinition?>> {
    if (elements.isEmpty()) throw InternalParserError()
    if (elements.size == 1) {
        return ParseResult.of(Pair(elements[0].asTerm(), null))
    }

    val leftmostOperatorWithMostPrecedence: Pair<Int, Set<OperatorDefinition>> = elements
        .asSequence()
        .mapIndexed { index, it -> Pair(index, it) }
        .filter {
            val element = it.second
            if (!it.second.hasTextContent) return@filter false

            if (element !is Atom) return@filter true
            return@filter !element.quoted // quoted atoms cannot be operators
        }
        .map { (index, tokenOrTerm) -> Pair(index, opRegistry.getOperatorDefinitionsFor(tokenOrTerm.textContent)) }
        .filter { it.second.isNotEmpty() }
        .maxByOrNull { it.second.maxByOrNull(OperatorDefinition::precedence)!!.precedence }
        ?: throw ExpressionASTBuildingException(SyntaxError("Operator expected", elements[0].location.end))

    val index = leftmostOperatorWithMostPrecedence.first

    // will store results that can be constructed but are not necessarily the best fit to the input
    // instead of failing with an exception, this one might be returned as a surrogate
    var preliminaryResult: ParseResult<Pair<Term, OperatorDefinition?>>? = null

    tryOperatorDefinitionForIndex@ for (operatorDef in leftmostOperatorWithMostPrecedence.second) {
        val reportings = mutableSetOf<Reporting>()

        if (operatorDef.type.isPrefix) {
            val rhsResult = buildExpressionAST(elements.subList(index + 1, elements.size), opRegistry)
            var thisTerm = CompoundTerm(
                operatorDef.name,
                if (rhsResult.item != null) arrayOf(rhsResult.item.first) else emptyArray()
            ).also { it.sourceInformation = elements[index].location..elements.last().location }

            if (operatorDef.type == FX && rhsResult.item?.second != null) {
                val rhsOp = rhsResult.item.second!!
                if (rhsOp.type == YFX && rhsOp.precedence >= operatorDef.precedence) {
                    val rhsCompound = rhsResult.item.first as CompoundTerm
                    thisTerm = CompoundTerm(
                        rhsOp.name,
                        arrayOf(
                            CompoundTerm(
                                operatorDef.name,
                                arrayOf(rhsCompound.arguments[0])
                            ).also { it.sourceInformation = elements[index].location..rhsCompound.arguments[1].location },
                            rhsCompound.arguments[1]
                        )
                    ).also { it.sourceInformation = elements[index].location..rhsCompound.arguments[1].location }
                }
                else if (rhsOp.type == YF) {
                    val rhsCompound = rhsResult.item.first as CompoundTerm
                    thisTerm = CompoundTerm(
                        rhsOp.name,
                        arrayOf(
                            CompoundTerm(
                                operatorDef.name,
                                arrayOf(rhsCompound.arguments[0])
                            ).also { it.sourceInformation = elements[index].location..rhsCompound.arguments[0].location }
                        )
                    ).also { it.sourceInformation = elements[index].location..rhsCompound.location }
                }
                else if (rhsOp.precedence >= operatorDef.precedence) {
                    reportings += SemanticError(
                        "Operator priority clash: right of ${operatorDef.name} must be strictly less precedence than ${operatorDef.precedence}, but found ${rhsOp.name} with precedence ${rhsOp.precedence}",
                        elements[index].location
                    )
                }
            }

            val hasLhs = index > 0
            if (hasLhs) {
                val newElements = ArrayList<TokenOrTerm>(index + 2)
                newElements.addAll(elements.subList(0, index))
                newElements.add(thisTerm)
                try {
                    val fullResult = buildExpressionAST(newElements, opRegistry)
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
                buildExpressionAST(elements.subList(0, index), opRegistry)
            } else {
                ParseResult(null, NOT_RECOGNIZED, setOf(SyntaxError("Missing left hand side operand", elements[index].location)))
            }

            val rhsResult = if (index < elements.lastIndex) {
                buildExpressionAST(elements.subList(index + 1, elements.size), opRegistry)
            } else {
                ParseResult(null, NOT_RECOGNIZED, setOf(SyntaxError("Missing right hand side operand", elements[index].location)))
            }

            var thisCompound = CompoundTerm(
                operatorDef.name,
                listOfNotNull(lhsResult.item?.first, rhsResult.item?.first).toTypedArray()
            ).also { it.sourceInformation = elements.first().location..elements.last().location }

            reportings.addAll(lhsResult.reportings)
            reportings.addAll(rhsResult.reportings)

            if (rhsResult.item?.second != null) {
                val rhsCompound = rhsResult.item.first as CompoundTerm
                val rhsOp = rhsResult.item.second!!
                if ((operatorDef.type == XFX || operatorDef.type == YFX) && rhsOp.precedence >= operatorDef.precedence) {
                    if (rhsOp.type == YFX) {
                        thisCompound = CompoundTerm(
                            rhsCompound.functor,
                            arrayOf(
                                CompoundTerm(
                                    operatorDef.name,
                                    arrayOf(lhsResult.item!!.first, rhsCompound.arguments[0])
                                ).also { it.sourceInformation = lhsResult.item.first.location..rhsCompound.arguments[0].location },
                                rhsCompound.arguments[1]
                            )
                        ).also { it.sourceInformation = lhsResult.item.first.location..rhsCompound.arguments[1].location }
                    }
                    else if (rhsOp.type == YF) {
                        thisCompound = CompoundTerm(
                            rhsCompound.functor,
                            arrayOf(
                                CompoundTerm(
                                    operatorDef.name,
                                    arrayOf(lhsResult.item!!.first, rhsCompound.arguments[0])
                                ).also { it.sourceInformation = lhsResult.item.first.location..rhsCompound.arguments[0].location }
                            )
                        ).also { it.sourceInformation = lhsResult.item.first.location..rhsCompound.location }
                    }
                    else {
                        reportings += SemanticError(
                            "Operator priority clash: right of ${operatorDef.name} must be strictly less precedence than ${operatorDef.precedence}, but found ${rhsOp.name} with precedence ${rhsOp.precedence}",
                            elements[index].location
                        )
                    }
                }
            }

            preliminaryResult = ParseResult(
                Pair(
                    thisCompound,
                    operatorDef
                ),
                MATCHED,
                reportings
            )

            if (lhsResult.isSuccess && rhsResult.isSuccess) {
                return preliminaryResult
            } // else: try another operator definition
        } else if (operatorDef.type.isPostfix) {
            val lhsResult = buildExpressionAST(elements.subList(0, index), opRegistry)
            val thisTerm = CompoundTerm(
                operatorDef.name,
                if (lhsResult.item != null) arrayOf(lhsResult.item.first) else emptyArray()
            ).also {
                it.sourceInformation = elements.first().location..elements[index].location
            }

            val hasRhs = elements.lastIndex > index
            if (hasRhs) {
                val newElements = ArrayList<TokenOrTerm>(index + 2)
                newElements.add(thisTerm)
                newElements.addAll(elements.subList(index + 1, elements.size))
                val fullResult = buildExpressionAST(newElements, opRegistry)
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
    throw ExpressionASTBuildingException(SemanticError("Cannot meaningfully use operator ${elements[index]}", elements[index].location))
}
