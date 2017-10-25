package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.*
import com.github.tmarsteel.ktprolog.parser.ParsedAtom
import com.github.tmarsteel.ktprolog.parser.ParsedList
import com.github.tmarsteel.ktprolog.parser.ParsedVariable
import com.github.tmarsteel.ktprolog.parser.lexer.*
import com.github.tmarsteel.ktprolog.parser.sequence.TransactionalSequence
import com.github.tmarsteel.ktprolog.parser.source.SourceUnit
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Variable
import io.kotlintest.specs.FreeSpec

class PrologParserTest : FreeSpec() {init{

    fun tokensOf(str: String): TransactionalSequence<Token> = Lexer(SourceUnit("testcode"), str.iterator())

    val parser = PrologParser()

    "comma separated terms" - {
        "1,2,3,4 a" {
            val tokens = tokensOf("1,2,3,4 followUpAtom")
            val result = parser.parseCommaSeparatedTerms(tokens, { true })
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item != null)
            result.item!!.size shouldEqual 4
            (result.item!![0] as Atom).name shouldEqual "1"
            (result.item!![1] as Atom).name shouldEqual "2"
            (result.item!![2] as Atom).name shouldEqual "3"
            (result.item!![3] as Atom).name shouldEqual "4"

            tokens.hasNext() shouldEqual true
            val followUpToken = tokens.next()
            assert(followUpToken is IdentifierToken)
            (followUpToken as IdentifierToken).textContent shouldEqual "followUpAtom"
        }

        "invalid: 1,2,." {
            val tokens = tokensOf("1,2,.")
            val result = parser.parseCommaSeparatedTerms(tokens, { it is OperatorToken && it.operator == Operator.FULL_STOP })

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1
            assert(result.item != null)
            result.item!!.size shouldEqual 2

            tokens.hasNext() shouldEqual true
            val followUpToken = tokens.next()
            assert(followUpToken is OperatorToken)
            (followUpToken as OperatorToken).operator shouldEqual Operator.FULL_STOP
        }
    }

    "atom" - {
        "a" {
            val result = parser.parseTerm(tokensOf("a"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAtom)
            (result.item!! as ParsedAtom).name shouldEqual "a"
        }

        "someAtom" {
            val result = parser.parseTerm(tokensOf("someAtom"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAtom)
            (result.item!! as ParsedAtom).name shouldEqual "someAtom"
        }

        "invalid: PARENT_OPEN" {
            val result = parser.parseTerm(tokensOf("("))
            result.certainty shouldEqual NOT_RECOGNIZED
            result.reportings.size shouldEqual 1
            result.item shouldBe null
        }

        "a," {
            val tokens = tokensOf("a,")
            val result = parser.parseTerm(tokens)
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAtom)
            (result.item!! as ParsedAtom).name shouldEqual "a"

            tokens.hasNext() shouldEqual true
            val followUpToken = tokens.next()
            assert(followUpToken is OperatorToken)
            (followUpToken as OperatorToken).operator shouldEqual Operator.COMMA
        }
    }

    "variable" - {
        "X" {
            val result = parser.parseTerm(tokensOf("X"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldEqual "X"
        }

        "Variable" {
            val result = parser.parseTerm(tokensOf("Variable"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldEqual "Variable"
        }

        "_underscore" {
            val result = parser.parseTerm(tokensOf("_underscore"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldEqual "_underscore"
        }
    }
    
    "predicate" - {
        "predicate(foo, X, bar)" {
            val result = parser.parseTerm(tokensOf("predicate(foo, X, bar)"))
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

        "invalid: missing closing parenthesis: EOF instead" {
            val result = parser.parseTerm(tokensOf("predicate(foo, X"))
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
        }

        "invalid: missing closing parenthesis: . instead" {
            val result = parser.parseTerm(tokensOf("predicate(foo, X."))
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
        }

        "invalid: missing comma" {
            val tokens = tokensOf("predicate(foo  X).")
            val result = parser.parseTerm(tokens)
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"

            val followupToken = tokens.next()
            assert(followupToken is OperatorToken)
            (followupToken as OperatorToken).operator shouldEqual Operator.FULL_STOP
        }
    }

    "list" - {
        "[1,2]" {
            val result = parser.parseList(tokensOf("[1,2]"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 2
            result.item!!.tail shouldBe null
        }

        "[1|T]" {
            val result = parser.parseList(tokensOf("[1|T]"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            assert(result.item!!.tail is ParsedVariable)
            (result.item!!.tail as ParsedVariable).name shouldEqual "T"
        }

        "[1,2|T]" {
            val result = parser.parseList(tokensOf("[1,2|T]"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 2
            assert(result.item!!.tail is ParsedVariable)
            (result.item!!.tail as ParsedVariable).name shouldEqual "T"
        }

        "invalid: [1," {
            val result = parser.parseList(tokensOf("[1,"))
            result.certainty shouldEqual MATCHED
            assert(result.reportings.size in 1..2)
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            result.item!!.tail shouldBe null
        }

        "invalid: [1,]" {
            val result = parser.parseList(tokensOf("[1,]"))
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            result.item!!.tail shouldBe null
        }

        "invalid: [1|]" {
            val result = parser.parseList(tokensOf("[1|]"))
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 1
            result.item!!.tail shouldBe null
        }

        "[1,2|[3,4]]" {
            val result = parser.parseList(tokensOf("[1,2|[3,4]]"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 4
            result.item!!.tail shouldBe null
        }

        "[1,2|[3|T]]" {
            val result = parser.parseList(tokensOf("[1,2|[3|T]]"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedList)
            result.item!!.elements.size shouldEqual 3
            assert(result.item!!.tail is ParsedVariable)
            (result.item!!.tail as ParsedVariable).name shouldEqual "T"
        }
    }
}}