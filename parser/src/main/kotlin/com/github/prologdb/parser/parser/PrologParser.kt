package com.github.prologdb.parser.parser

import com.github.prologdb.parser.*
import com.github.prologdb.parser.lexer.*
import com.github.prologdb.parser.lexer.Operator.*
import com.github.prologdb.parser.lexer.TokenType.IDENTIFIER
import com.github.prologdb.parser.lexer.TokenType.NUMERIC_LITERAL
import com.github.prologdb.parser.parser.ParseResultCertainty.MATCHED
import com.github.prologdb.parser.parser.ParseResultCertainty.NOT_RECOGNIZED
import com.github.prologdb.parser.sequence.TransactionalSequence
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.HasPrologSource
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.knowledge.library.OperatorType.*
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*

/** If kotlin had union types this would be `Token | Term` */
private typealias TokenOrTerm = Any

class PrologParser {

    fun parseQuery(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Query> {
        return parseQuery(tokens, opRegistry, stopAtOperator(FULL_STOP))
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
            this::parseParenthesised,
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

    fun parseCompoundTerm(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<ParsedCompoundTerm> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("predicate invocation")))

        tokens.mark()

        val functorToken = tokens.next()

        if (functorToken !is IdentifierToken && functorToken !is OperatorToken) {
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

        val functor = functorToken.textContent!!

        // detect predicate/0 invocations
        if (!tokens.hasNext()) {
            tokens.rollback() // peek of PARENT_OPEN
            tokens.rollback() // mark() at start of method
            return ParseResult(
                ParsedCompoundTerm(
                    functor,
                    emptyArray(),
                    functorToken.location..parentOpenToken.location
                ),
                MATCHED,
                setOf(UnexpectedEOFError("[compound term arguments]", "closing parenthesis"))
            )
        }

        val tokenAfterParentOpen = tokens.next()
        if (tokenAfterParentOpen is OperatorToken && tokenAfterParentOpen.operator == PARENT_CLOSE) {
            tokens.commit() // peek of PARENT_OPEN
            tokens.commit() // mark() at start of method
            return ParseResult.of(
                ParsedCompoundTerm(
                    functor,
                    emptyArray(),
                    functorToken.location..tokenAfterParentOpen.location
                )
            )
        }
        tokens.rollback() // rollback to before PARENT_OPEN

        // arguments
        val argsTermResult = parseParenthesised(tokens, opRegistry, true)
        if (argsTermResult.isSuccess) {
            tokens.commit()
            val argsResult = commaCompoundToList(argsTermResult.item!!)
            return ParseResult(
                ParsedCompoundTerm(functor, argsResult.item!!.toTypedArray(), functorToken.location..argsResult.item.last().location),
                    MATCHED,
                    argsTermResult.reportings + argsResult.reportings
            )
        }
        else {
            tokens.rollback()
            return ParseResult(
                ParsedCompoundTerm(
                    functor,
                    emptyArray(),
                    functorToken.location..parentOpenToken.location
                ),
                NOT_RECOGNIZED,
                argsTermResult.reportings
            )
        }
    }

    fun parseList(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<ParsedList> {
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
                ParsedList(emptyList(), null, openingBracketToken.location..token.location),
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
                ParsedList(elements, null, openingBracketToken.location..elementsResult.item.location),
                MATCHED,
                elementsResult.reportings + UnexpectedEOFError(HEAD_TAIL_SEPARATOR, BRACKET_CLOSE)
            )
        }

        tokens.mark()
        val tokenAfterElements = tokens.next()
        var tail: Term?
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
            ParsedList(elements, tail, openingBracketToken.location..listEndLocation),
            MATCHED,
            reportings
        )
    }

    fun parseDictionary(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<ParsedDictionary> {
        // copied from parseList and adapted

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
            return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError("dict content or ${CURLY_CLOSE}")))
        }
        tokens.mark()
        var token = tokens.next()
        if (token is OperatorToken && token.operator == CURLY_CLOSE) {
            tokens.commit()
            tokens.commit()
            return ParseResult(
                ParsedDictionary(emptyMap(), null,openingCurlyToken.location..token.location),
                MATCHED,
                emptySet()
            )
        }
        // else: dict with content
        tokens.rollback()

        val elementsResult = parseTerm(tokens, opRegistry, { t -> stopAtOperator(HEAD_TAIL_SEPARATOR)(t) || stopAtOperator(CURLY_CLOSE)(t) })
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
            reportings.add(SyntaxError("Keys in dict pairs must be atoms", (it as ParsedCompoundTerm).arguments[0].location))
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
                ParsedDictionary(pairsAsKotlinMap, null, openingCurlyToken.location..elementsResult.item.location),
                MATCHED,
                elementsResult.reportings + UnexpectedEOFError(HEAD_TAIL_SEPARATOR, CURLY_CLOSE)
            )
        }

        tokens.mark()
        val tokenAfterElements = tokens.next()
        var tail: Term?
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
            ParsedDictionary(pairsAsKotlinMap, tail, openingCurlyToken.location..dictEndLocation),
            MATCHED,
            reportings
        )
    }

    fun parseParenthesised(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry): ParseResult<Term> {
        return parseParenthesised(tokens, opRegistry, false)
    }

    /**
     * Parses a parenthesised term: `(term)`.
     * @param outmostWithoutProtection If the term within the parenthesis is a compound, does not set the [CompoundTerm.parenthesisProtection] flag.
     */
    fun parseParenthesised(tokens: TransactionalSequence<Token>, opRegistry: OperatorRegistry, outmostWithoutProtection: Boolean): ParseResult<Term> {
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
            val item = termResult.item
            if (!outmostWithoutProtection && item is CompoundTerm) {
                if (item !is ParsedCompoundTerm) throw InternalParserError()
                item.parenthesisProtection = true
            }

            return ParseResult(
                item,
                MATCHED,
                reportings + termResult.reportings
            )
        }
    }

    fun parseAtomicOrVariable(tokens: TransactionalSequence<Token>): ParseResult<Term> {
        if (!tokens.hasNext()) return ParseResult(null, NOT_RECOGNIZED, setOf(UnexpectedEOFError(IDENTIFIER)))

        tokens.mark()

        var token = tokens.next()

        if (token is IdentifierToken) {
            tokens.commit()

            if (token.textContent == "_") {
                return ParseResult(
                    ParsedAnonymousVariable(token.location),
                    MATCHED,
                    emptySet()
                )
            }
            else if (token.textContent[0].isUpperCase() || token.textContent[0] == '_') {
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
                is Int -> PrologInteger(tokenNumber.toLong(), token.location)
                is Long -> PrologInteger(tokenNumber, token.location)
                is Float -> PrologDecimal(tokenNumber.toDouble(), token.location)
                is Double -> PrologDecimal(tokenNumber, token.location)
                else -> throw InternalParserError("Unsupported number type in numeric literal token")
            }

            return ParseResult(
                    number,
                    MATCHED,
                    emptySet()
            )
        }
        else if (token is StringLiteralToken) {
            tokens.commit()

            return ParseResult(
                ParsedPrologString(
                    token.content,
                    token.location
                ),
                MATCHED,
                emptySet()
            )
        }
        else if (token is AtomLiteralToken) {
            tokens.commit()
            return ParseResult(
                ParsedAtom(
                    token.name,
                    token.location
                ),
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
     * Parse the given tokens as a module file.
     */
    fun parseModule(
        tokens: TransactionalSequence<Token>,
        contextOperators: OperatorRegistry = ISOOpsOperatorRegistry
    ): ParseResult<Module> {
        val moduleLocalOperators = DefaultOperatorRegistry()
        moduleLocalOperators.include(contextOperators)

        val clauses = mutableListOf<Clause>()
        val dynamics = mutableSetOf<ClauseIndicator>()
        var moduleDeclaration: ModuleDeclaration? = null
        var moduleDeclarationSeen = false
        var imports = mutableListOf<ModuleImport>()
        val reportings = mutableSetOf<Reporting>()

        var moduleDeclarationNotFirstStatementErrorEmitted = false
        fun emitModuleDeclarationNotFirstStatementError(onLocation: SourceLocationRange) {
            if (moduleDeclarationNotFirstStatementErrorEmitted) return

            reportings.add(SemanticError(
                "The module/1 directive must be the first statement in the file.",
                onLocation
            ))

            moduleDeclarationNotFirstStatementErrorEmitted = true
        }

        /**
         * Adds the given operator definition to the library.
         * @param opDefinitionAST The definition, e.g.: `op(400,xf,isDead)`
         */
        fun handleOperator(opDefinitionAST: ParsedCompoundTerm): OperatorDefinition? {
            val priorityArgument = opDefinitionAST.arguments[0]
            if (priorityArgument !is com.github.prologdb.runtime.term.PrologNumber || !priorityArgument.isInteger) {
                reportings.add(SemanticError("operator priority must be an integer", priorityArgument.location))
                return null
            }

            val precedenceAsLong = (priorityArgument as com.github.prologdb.runtime.term.PrologNumber).toInteger()
            if (precedenceAsLong < 0 || precedenceAsLong > 1200) {
                reportings.add(SemanticError("operator precedence must be between 0 and 1200 (inclusive)", opDefinitionAST.arguments[0].location))
                return null
            }
            val precedence = precedenceAsLong.toShort()

            if (opDefinitionAST.arguments[1] !is Atom) {
                reportings.add(SemanticError("atom expected but found ${opDefinitionAST.arguments[1]}", opDefinitionAST.arguments[1].location))
                return null
            }

            val typeAsUCString = (opDefinitionAST.arguments[1] as Atom).name.toUpperCase()
            val operatorType = try {
                OperatorType.valueOf(typeAsUCString)
            }
            catch (ex: IllegalArgumentException) {
                reportings.add(SemanticError("${typeAsUCString.toLowerCase()} is not a known operator type", opDefinitionAST.arguments[1].location))
                return null
            }

            if (opDefinitionAST.arguments[2] !is Atom) {
                reportings.add(SemanticError("Atom expected but got ${opDefinitionAST.arguments[2]}", opDefinitionAST.arguments[2].location))
                return null
            }

            val definition = OperatorDefinition(precedence, operatorType, (opDefinitionAST.arguments[2] as Atom).name)
            moduleLocalOperators.defineOperator(definition)

            return definition
        }

        /**
         * The dynamic/1 builtin
         * @param the only parameter to dynamic/1
         */
        fun handleDynamicDirective(indicatorTerm: Term) {
            if (indicatorTerm !is ParsedCompoundTerm || indicatorTerm.arity != 2 || indicatorTerm.functor != "/") {
                reportings.add(SemanticError(
                    "The argument to dynamic/1 must be an instance of `/`/2",
                    indicatorTerm.location
                ))
                return
            }

            val indicator = ClauseIndicator.fromIdiomatic(indicatorTerm, reportings)

            indicator?.let { dynamics.add(it) }
        }

        fun handleModuleDeclaration(invocation: CompoundTerm) {
            val args = invocation.arguments
            if (args.size !in 1..2) {
                reportings.add(SemanticError(
                    "Directive module/${args.size} is not defined.",
                    invocation.location
                ))
                return
            }

            if (moduleDeclarationSeen) {
                reportings.add(SemanticError(
                    "Cannot declare module more than once.",
                    invocation.location
                ))
                return
            }

            moduleDeclarationSeen = true

            if (args[0] !is Atom) {
                reportings.add(SemanticError(
                    "Argument 0 to module/${args.size} must be an atom, got ${args[0].prologTypeName}",
                    args[0].location
                ))
                return
            }

            val name = (args[0] as Atom).name
            var exportSelection: Set<ClauseIndicator>? = null

            if (args.size == 2) {
                if (args[1] is PrologList) {
                    exportSelection = (args[1] as PrologList).elements
                        .mapNotNull {
                            ClauseIndicator.fromIdiomatic(it, reportings)
                        }
                        .toSet()
                } else {
                    reportings.add(SemanticError(
                        "Argument 1 to module/2 must be a list, got ${args[1].prologTypeName}",
                        args[1].location
                    ))
                }
            }

            moduleDeclaration = ModuleDeclaration(name, exportSelection)
        }

        fun handleUseModuleDirective(invocation: CompoundTerm) {
            try {
                imports.add(ModuleImport.fromUseModuleSyntax(invocation.arguments))
            } catch (ex: PrologRuntimeException) {
                reportings.add(SemanticError(
                    ex.message ?: "Invalid import directive",
                    invocation.location
                ))
            }
        }

        fun handleDirective(command: ParsedCompoundTerm) {
            if (!moduleDeclarationSeen && command.functor != "module") {
                emitModuleDeclarationNotFirstStatementError(command.location)
            }

            when(command.arity) {
                3 -> when(command.functor) {
                    "op" -> handleOperator(command)
                }
                2 -> when (command.functor) {
                    "module" -> return handleModuleDeclaration(command)
                    "use_module" -> return handleUseModuleDirective(command)
                }
                1 -> when(command.functor) {
                    "dynamic" -> return handleDynamicDirective(command.arguments[0])
                    "module" -> return handleModuleDeclaration(command)
                    "use_module" -> return handleUseModuleDirective(command)
                }
            }

            reportings += SemanticError(
                "Directive ${command.functor}/${command.arity} is not defined.",
                command.location
            )
        }

        fun handleRule(definition: ParsedCompoundTerm) {
            val head = definition.arguments[0] as? ParsedCompoundTerm ?: throw InternalParserError("Rule heads must be compound term")
            val queryTerm = definition.arguments[1] as? ParsedCompoundTerm ?: throw InternalParserError("Queries must be compound term")
            val transformResult = transformQuery(queryTerm)

            if (transformResult.item != null) {
                clauses.add(ParsedRule(head, transformResult.item, head.location..queryTerm.location))
            }

            reportings += transformResult.reportings
        }

        while (tokens.hasNext()) {
            val parseResult = parseTerm(tokens, moduleLocalOperators, stopAtOperator(FULL_STOP))
            reportings += parseResult.reportings

            if (parseResult.isSuccess) {
                val item = parseResult.item ?: throw InternalParserError("Result item should not be null")
                if (item is CompoundTerm) {
                    item as? ParsedCompoundTerm ?: throw InternalParserError("Expected ParsedCompoundTerm, got CompoundTerm")
                    // detect directive
                    if (item.isDirectiveInvocation) {
                        handleDirective(item.arguments[0] as ParsedCompoundTerm)
                    }
                    else if (item.isRuleDefinition) {
                        if (!moduleDeclarationSeen) emitModuleDeclarationNotFirstStatementError(item.location)
                        handleRule(item)
                    }
                    else {
                        if (!moduleDeclarationSeen) emitModuleDeclarationNotFirstStatementError(item.location)
                        clauses.add(item)
                    }
                } else {
                    if (!moduleDeclarationSeen) emitModuleDeclarationNotFirstStatementError(item.location)
                    reportings += SemanticError("A ${item.prologTypeName} is not a top level declaration, expected a compound term.", item.location)
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

        if (moduleDeclaration == null) {
            reportings += SemanticError(
                "Missing module declaration",
                SourceLocationRange(SourceLocation.EOF, SourceLocation.EOF)
            )

            return ParseResult(
                null,
                MATCHED,
                reportings
            )
        }

        return ParseResult(
            ASTModule(
                moduleDeclaration!!.moduleName,
                imports,
                clauses,
                dynamics,
                moduleDeclaration!!.exportedPredicates
                    ?: clauses.map { ClauseIndicator.of(it) }.toSet()
            ),
            MATCHED,
            reportings
        )
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
        while (pivot is CompoundTerm && !(pivot as ParsedCompoundTerm).parenthesisProtection && pivot.arity == 2 && pivot.functor == Operator.COMMA.text) {
            pivot as? ParsedCompoundTerm ?: throw InternalParserError()
            list.add(pivot.arguments[0])
            pivot = pivot.arguments[1]
        }

        list.add(pivot)

        return ParseResult.of(list)
    }

    /**
     * Converts a term given as the second argument to `:-/2` into an instance of [Query].
     */
    private fun transformQuery(query: Term): ParseResult<Query> {
        if (query is ParsedCompoundTerm) {
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

                while (pivot is ParsedCompoundTerm && pivot.arity == 2 && pivot.functor == operator) {
                    addElement(pivot.arguments[0])
                    pivot = pivot.arguments[1]
                }
                addElement(pivot)

                return ParseResult(
                    if (operator == Operator.COMMA.text) {
                        ParsedAndQuery(
                            elements.toTypedArray(),
                            query.location
                        ) as Query
                    } else {
                        ParsedOrQuery(
                            elements.toTypedArray(),
                            query.location
                        ) as Query
                    },
                    MATCHED,
                    reportings
                )
            } else {
                return ParseResult.of(ParsedPredicateInvocationQuery(query))
            }
        } else {
            return ParseResult(null, NOT_RECOGNIZED, setOf(SemanticError("$query is not a valid query component", query.location)))
        }
    }

    companion object {
        /**
         * Helper function for the `shouldStop` parameter to [parseTerm].
         * @return Aborts matching if the next token in the lazysequence is an [OperatorToken] with the given [Operator], otherwise false.
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

        /**
         * Helper for the `shouldStop` parameter to [parseTerm].
         * @return Aborts matching if EOF is reached.
         */
        val STOP_AT_EOF: (TransactionalSequence<Token>) -> Boolean = { !it.hasNext() }
    }
}

private val ParsedCompoundTerm.isDirectiveInvocation: Boolean
    get() = functor == HEAD_QUERY_SEPARATOR.text && arity == 1 && arguments[0] is CompoundTerm

private val ParsedCompoundTerm.isRuleDefinition: Boolean
    get() = functor == HEAD_QUERY_SEPARATOR.text && arity == 2 && arguments[0] is CompoundTerm

/**
 * Skips (`next()`s) tokens in the receiver lazysequence until the parenthesis + bracket levels are 0 and the given
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
        is HasPrologSource -> sourceInformation as? SourceLocationRange
            ?: throw InternalParserError()
        else -> throw InternalParserError()
    }

private fun TokenOrTerm.asTerm(): Term {
    if (this is Term) return this

    if (this is Token && this is OperatorToken) {
        val text = this.textContent ?: throw InternalParserError()
        return ParsedAtom(text, this.location)
    }

    throw InternalParserError()
}

private class ExpressionASTBuildingException(reporting: Reporting) : ReportingException(reporting)

/**
 * @return The parsed term and the [OperatorDefinition] of its operator; if the parsed term does not involve an
 *         operator, the operator is null
 * @throws ExpressionASTBuildingException
 */
private fun buildExpressionAST(elements: List<TokenOrTerm>, opRegistry: OperatorRegistry): ParseResult<Pair<Term,OperatorDefinition?>> {
    if (elements.isEmpty()) throw InternalParserError()
    if (elements.size == 1) {
        return ParseResult.of(Pair(elements[0].asTerm(), null))
    }

    val leftmostOperatorWithMostPrecedence: Pair<Int, Set<OperatorDefinition>> = elements
        .mapIndexed { index, it -> Pair(index, it) }
        .filter { it.second.hasTextContent }
        .map { (index, tokenOrTerm) -> Pair(index, opRegistry.getOperatorDefinitionsFor(tokenOrTerm.textContent)) }
        .filter { it.second.isNotEmpty() }
        .maxBy { it.second.maxBy(OperatorDefinition::precedence)!!.precedence }
        ?: throw ExpressionASTBuildingException(SyntaxError("Operator expected", elements[0].location.end))

    val index = leftmostOperatorWithMostPrecedence.first

    // will store results that can be constructed but are not necessarily the best fit to the input
    // instead of failing with an exception, this one might be returned as a surrogate
    var preliminaryResult: ParseResult<Pair<Term,OperatorDefinition?>>? = null

    tryOperatorDefinitionForIndex@ for (operatorDef in leftmostOperatorWithMostPrecedence.second) {
        val reportings = mutableSetOf<Reporting>()

        if (operatorDef.type.isPrefix) {
            val rhsResult = buildExpressionAST(elements.subList(index + 1, elements.size), opRegistry)
            var thisTerm = ParsedCompoundTerm(
                operatorDef.name,
                if (rhsResult.item != null) arrayOf(rhsResult.item.first) else emptyArray(),
                elements[index].location..elements.last().location
            )

            if (operatorDef.type == FX && rhsResult.item?.second != null) {
                val rhsOp = rhsResult.item.second!!
                if (rhsOp.type == YFX) {
                    val rhsCompound = rhsResult.item.first as ParsedCompoundTerm
                    thisTerm = ParsedCompoundTerm(
                        rhsOp.name,
                        arrayOf(
                            ParsedCompoundTerm(
                                operatorDef.name,
                                arrayOf(rhsCompound.arguments[0]),
                                elements[index].location..rhsCompound.arguments[1].location
                            ),
                            rhsCompound.arguments[1]
                        ),
                        elements[index].location..rhsCompound.arguments[1].location
                    )
                }
                else if (rhsOp.type == YF) {
                    val rhsCompound = rhsResult.item.first as ParsedCompoundTerm
                    thisTerm = ParsedCompoundTerm(
                        rhsOp.name,
                        arrayOf(
                            ParsedCompoundTerm(
                                operatorDef.name,
                                arrayOf(rhsCompound.arguments[0]),
                                elements[index].location..rhsCompound.arguments[0].location
                            )
                        ),
                        elements[index].location..rhsCompound.location
                    )
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

            var thisCompound = ParsedCompoundTerm(
                operatorDef.name,
                listOf(lhsResult.item?.first, rhsResult.item?.first).filterNotNull().toTypedArray(),
                elements.first().location..elements.last().location
            )

            reportings.addAll(lhsResult.reportings)
            reportings.addAll(rhsResult.reportings)

            if (rhsResult.item?.second != null) {
                val rhsCompound = rhsResult.item.first as ParsedCompoundTerm
                val rhsOp = rhsResult.item.second!!
                if ((operatorDef.type == XFX || operatorDef.type == YFX) && rhsOp.precedence >= operatorDef.precedence) {
                    if (rhsOp.type == YFX) {
                        thisCompound = ParsedCompoundTerm(
                            rhsCompound.functor,
                            arrayOf(
                                ParsedCompoundTerm(
                                    operatorDef.name,
                                    arrayOf(lhsResult.item!!.first, rhsCompound.arguments[0]),
                                    lhsResult.item.first.location..rhsCompound.arguments[0].location
                                ),
                                rhsCompound.arguments[1]
                            ),
                            lhsResult.item.first.location..rhsCompound.arguments[1].location
                        )
                    }
                    else if (rhsOp.type == YF) {
                        thisCompound = ParsedCompoundTerm(
                            rhsCompound.functor,
                            arrayOf(
                                ParsedCompoundTerm(
                                    operatorDef.name,
                                    arrayOf(lhsResult.item!!.first, rhsCompound.arguments[0]),
                                    lhsResult.item.first.location..rhsCompound.arguments[0].location
                                )
                            ),
                            lhsResult.item.first.location..rhsCompound.location
                        )
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
            val thisTerm = ParsedCompoundTerm(
                operatorDef.name,
                if (lhsResult.item != null) arrayOf(lhsResult.item.first) else emptyArray(),
                elements.first().location..elements[index].location
            )

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

fun ClauseIndicator.Companion.fromIdiomatic(indicator: Term, reportings: MutableCollection<in Reporting>): ClauseIndicator? {
    if (indicator !is CompoundTerm) {
        reportings += SemanticError(
            "Clause indicators must be instances of `/`/2, got ${indicator.prologTypeName}",
            indicator.location
        )
        return null
    }

    val name = indicator.arguments[0]
    val arity = indicator.arguments[1]

    if (name !is Atom) {
        reportings += SemanticError(
            "Argument 0 to `/`/2 must be an atom",
            name.location
        )
        return null
    }

    if (arity !is PrologNumber || !arity.isInteger) {
        reportings += SemanticError(
            "Argument 1 to `/`/2 must be an integer",
            arity.location
        )
        return null
    }

    val arityValue = arity.toInteger()
    if (arityValue < 0L || arityValue > Integer.MAX_VALUE.toLong()) {
        reportings += SemanticError(
            "Argument 1 to `/`/2 must be an integer in the range [0; ${Int.MAX_VALUE}]",
            arity.location
        )
        return null
    }

    return ClauseIndicator.of(name.name, arityValue.toInt())
}
