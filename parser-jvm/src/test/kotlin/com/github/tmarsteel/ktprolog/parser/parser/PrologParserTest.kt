package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.knowledge.library.DefaultOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorDefinition
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorType
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.MATCHED
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.NOT_RECOGNIZED
import com.github.tmarsteel.ktprolog.parser.ParsedAtom
import com.github.tmarsteel.ktprolog.parser.ParsedList
import com.github.tmarsteel.ktprolog.parser.ParsedPredicate
import com.github.tmarsteel.ktprolog.parser.ParsedVariable
import com.github.tmarsteel.ktprolog.parser.lexer.Lexer
import com.github.tmarsteel.ktprolog.parser.lexer.Token
import com.github.tmarsteel.ktprolog.parser.sequence.TransactionalSequence
import com.github.tmarsteel.ktprolog.parser.source.SourceUnit
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Variable
import io.kotlintest.matchers.*
import io.kotlintest.specs.FreeSpec

class PrologParserTest : FreeSpec() {
    override val oneInstancePerTest = true

    init{

    val operators = DefaultOperatorRegistry(true)
    operators.defineOperator(OperatorDefinition(500, OperatorType.XFX, "infixOpXFX500"))
    operators.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefixOpFY200"))

    fun tokensOf(str: String): TransactionalSequence<Token> = Lexer(SourceUnit("testcode"), str.iterator())

    fun parseTerm(tokens: TransactionalSequence<Token>) = PrologParser().parseTerm(tokens, operators, PrologParser.STOP_AT_EOF)
    fun parseTerm(code: String) = parseTerm(tokensOf(code))

    fun parseList(tokens: TransactionalSequence<Token>) = PrologParser().parseList(tokens, operators)
    fun parseList(code: String) = parseList(tokensOf(code))

    fun parseParenthesised(tokens: TransactionalSequence<Token>) = PrologParser().parseParenthesised(tokens, operators)
    fun parseParenthesised(code: String) = parseParenthesised(tokensOf(code))

    "atom" - {
        "a" {
            val result = parseTerm("a")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAtom)
            (result.item!! as ParsedAtom).name shouldEqual "a"
        }

        "someAtom" {
            val result = parseTerm("someAtom")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAtom)
            (result.item!! as ParsedAtom).name shouldEqual "someAtom"
        }

        "invalid: PARENT_OPEN" {
            val result = parseTerm("(")
            result.certainty shouldEqual NOT_RECOGNIZED
            result.reportings.size shouldEqual 1
            result.item shouldBe null
        }
    }

    "variable" - {
        "X" {
            val result = parseTerm("X")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldEqual "X"
        }

        "Variable" {
            val result = parseTerm("Variable")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldEqual "Variable"
        }

        "_underscore" {
            val result = parseTerm("_underscore")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldEqual "_underscore"
        }
    }

    "simple comma separated atoms" {
        val result = parseTerm("a, b, c")
        result.certainty shouldEqual MATCHED
        result.reportings should beEmpty()

        var predicate = result.item!! as ParsedPredicate
        predicate.name shouldEqual ","
        predicate.arity shouldEqual 2

        (predicate.arguments[0] as ParsedAtom).name shouldEqual "a"

        predicate = predicate.arguments[1] as ParsedPredicate
        predicate.name shouldEqual ","
        predicate.arity shouldEqual  2

        (predicate.arguments[0] as ParsedAtom).name shouldEqual "b"
        (predicate.arguments[1] as ParsedAtom).name shouldEqual "c"
    }
    
    "predicate" - {
        "invocation without arguments" {
            val result = parseTerm("predicate()")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
            predicate.arity shouldEqual 0
        }

        "invocation with three arguments" {
            val result = parseTerm("predicate(foo, X, bar)")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
            predicate.arguments.size shouldEqual 3

            assert(predicate.arguments[0] is Atom)
            (predicate.arguments[0] as Atom).name shouldEqual "foo"

            assert(predicate.arguments[1] is Variable)
            (predicate.arguments[1] as Variable).name shouldEqual "X"

            assert(predicate.arguments[2] is Atom)
            (predicate.arguments[2] as Atom).name shouldEqual "bar"
        }

        "invalid invocation: missing closing parenthesis: EOF instead" {
            val result = parseTerm("predicate(foo, X")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
        }

        "invalid invocation: missing closing parenthesis: . instead" {
            val result = parseTerm("predicate(foo, X.")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
        }

        "invalid: missing comma" {
            TODO()
        }

        "infix" {
            val result = parseTerm("a infixOpXFX500 b")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val predicate = result.item!! as ParsedPredicate
            predicate.name shouldEqual "infixOpXFX500"
            predicate.arguments.size shouldEqual 2
        }

        "invocation with infix as sole parameter" {
            val result = parseTerm("a(b infixOpXFX500 c)")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val predicate = result.item!! as ParsedPredicate
            predicate.name shouldEqual "a"
            predicate.arity shouldEqual 1

            val soleArg = predicate.arguments[0] as ParsedPredicate
            soleArg.name shouldEqual "infixOpXFX500"
            soleArg.arity shouldEqual 2
            soleArg.arguments[0].toString() shouldEqual "b"
            soleArg.arguments[1].toString() shouldEqual "c"
        }

        "infix with prefix as a parameter" {
            val result = parseTerm("prefixOpFY200 a infixOpXFX500 b")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as Predicate
            item.name shouldEqual "infixOpXFX500"
            item.arity shouldEqual 2

            val lhs = item.arguments[0] as Predicate
            lhs.name shouldEqual "prefixOpFY200"
            lhs.arity shouldEqual 1
        }
    }

    "list" - {
        "[]" {
            val tokens = tokensOf("[]")
            val result = parseList(tokens)
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 0
            result.item!!.tail shouldBe null

            shouldThrow<IllegalStateException> {
                tokens.commit()
            }
        }

        "[1,2]" {
            val result = parseList(tokensOf("[1,2]"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 2
            result.item!!.tail shouldBe null
        }

        "[1|T]" {
            val result = parseList("[1|T]")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            assert(result.item!!.tail is ParsedVariable)
            (result.item!!.tail as ParsedVariable).name shouldEqual "T"
        }

        "[1,2|T]" {
            val result = parseList("[1,2|T]")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 2
            assert(result.item!!.tail is ParsedVariable)
            (result.item!!.tail as ParsedVariable).name shouldEqual "T"
        }

        "invalid: [1," {
            val result = parseList("[1,")
            result.certainty shouldEqual MATCHED
            assert(result.reportings.size in 1..2)
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            result.item!!.tail shouldBe null
        }

        "invalid: [1,]" {
            val result = parseList("[1,]")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            result.item!!.tail shouldBe null
        }

        "invalid: [1|]" {
            val result = parseList("[1|]")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            result.item!!.tail shouldBe null
        }

        "[1,2|[3,4]]" {
            val result = parseList("[1,2|[3,4]]")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 4
            result.item!!.tail shouldBe null
        }

        "[1,2|[3|T]]" {
            val result = parseList("[1,2|[3|T]]")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 3
            assert(result.item!!.tail is ParsedVariable)
            (result.item!!.tail as ParsedVariable).name shouldEqual "T"
        }
    }
}}