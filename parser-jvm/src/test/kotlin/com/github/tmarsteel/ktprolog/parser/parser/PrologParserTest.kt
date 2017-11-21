package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.knowledge.DefaultOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorRegistry
import com.github.tmarsteel.ktprolog.parser.*
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.*
import com.github.tmarsteel.ktprolog.parser.lexer.*
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

    fun tokensOf(str: String): TransactionalSequence<Token> = Lexer(SourceUnit("testcode"), str.iterator())

    fun parseTerm(tokens: TransactionalSequence<Token>) = PrologParser().parseTerm(tokens, DefaultOperatorRegistry(true), PrologParser.STOP_AT_EOF)
    fun parseTerm(code: String) = parseTerm(tokensOf(code))

    fun parseList(tokens: TransactionalSequence<Token>) = PrologParser().parseList(tokens, DefaultOperatorRegistry(true))
    fun parseList(code: String) = parseList(tokensOf(code))

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

        "a," {
            TODO()
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
    
    "predicate" - {
        "predicate(foo, X, bar)" {
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

        "invalid: missing closing parenthesis: EOF instead" {
            val result = parseTerm("predicate(foo, X")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
        }

        "invalid: missing closing parenthesis: . instead" {
            val result = parseTerm("predicate(foo, X.")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1

            val predicate = result.item!! as Predicate
            predicate.name shouldEqual "predicate"
        }

        "invalid: missing comma" {
            TODO()
        }

        "a predicate b" {
            val result = parseTerm("a predicate b")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val predicate = result.item!! as ParsedPredicate
            predicate.name shouldEqual "predicate"
            predicate.arguments.size shouldEqual 2
        }

        "a(a predicate b)" {
            val result = parseTerm("a(a predicate b)")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0
        }

        "-(1) + 3" {
            val result = parseTerm("-(1) + 3")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as Predicate
            item.name shouldEqual "+"
            item.arguments.size shouldEqual 2

            val lhs = item.arguments[0] as Predicate
            lhs.name shouldEqual "-"
            lhs.arguments.size shouldEqual 1
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

        /*
    "query" - {
        "single predicate" {
            val result = parser.parseQuery(tokensOf("a(a)"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedPredicateQuery)
        }

        "a(a), b(b)" {
            val result = parser.parseQuery(tokensOf("a(a), b(b)"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAndQuery)
        }

        "a(a), b(b); c(c)" {
            val result = parser.parseQuery(tokensOf("a(a), b(b); c(c)"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedOrQuery)
        }

        "parenthesised 1" {
            val result = parser.parseQuery(tokensOf("(a(a))"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedPredicateQuery)
        }

        "parenthesised 2" {
            val result = parser.parseQuery(tokensOf("(a(a), b(b))"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAndQuery)
        }

        "parenthesised 3" {
            val result = parser.parseQuery(tokensOf("(a(a), b(b));(c(c), d(d))"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedOrQuery)
        }

        "infix" {
            val result = parser.parseQuery(tokensOf("X = a, f(x)"))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedAndQuery)

            val query = result.item!! as ParsedAndQuery
            assert(query.goals[0] is ParsedPredicateQuery)
            assert(query.goals[1] is ParsedPredicateQuery)

            (query.goals[0] as ParsedPredicateQuery).predicate.name shouldEqual "="
            (query.goals[0] as ParsedPredicateQuery).predicate.arguments.size shouldEqual 2
        }
    }

    "rule" - {
        "f(X, a) :- g(X), f(X)." {
            val result = parser.parseRule(tokensOf("f(X, a) :- g(X), f(X)."))
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedRule)
        }
    }*/


    /*"library" {
        "case 1" {
            val tokens = tokensOf("a([], _, []).")
            val result = parser.parseLibrary(tokens)
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()

            val predicate = result.item as Library
            predicate.exports.count() shouldEqual 1
            (predicate.exports.first() as Predicate).arguments.size shouldEqual 3
        }
    }*/
}}