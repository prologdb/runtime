package com.github.tmarsteel.ktprolog.parser.parser

import com.github.tmarsteel.ktprolog.builtin.EqualityLibrary
import com.github.tmarsteel.ktprolog.knowledge.Rule
import com.github.tmarsteel.ktprolog.knowledge.library.*
import com.github.tmarsteel.ktprolog.parser.ParseResultCertainty.MATCHED
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
    operators.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfixOpXF200"))

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
        }

        "invalid invocation: missing closing parenthesis: . instead" {
            val result = parseTerm("predicate(foo, X.")
            result.reportings shouldNot beEmpty()
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

        "prefix op" {
            val result = parseTerm("prefixOpFY200 b")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as Predicate
            item.name shouldEqual "prefixOpFY200"
            item.arity shouldEqual 1
        }

        "rule head with postfix in head" {
            val result = parseTerm("a postfixOpXF200 infixOpXFX500 b")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as Predicate
            item.name shouldEqual "infixOpXFX500"
            item.arity shouldEqual 2

            val lhs = item.arguments[0] as Predicate
            lhs.name shouldEqual "postfixOpXF200"
            lhs.arity shouldEqual 1

            val rhs = item.arguments[1] as Atom
            rhs.name shouldEqual "b"
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
            result.reportings shouldNot beEmpty()
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

    "expression term" - {
        "double prefix" {
            val result = parseTerm("prefixOpFY200 prefixOpFY200 a")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            var item = result.item!! as Predicate
            item.name shouldEqual "prefixOpFY200"
            item.arity shouldEqual 1

            item = item.arguments[0] as Predicate
            item.name shouldEqual "prefixOpFY200"
            item.arity shouldEqual 1
        }

        "precedence tests" - {
            operators.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefixOpFX200"))
            operators.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefixOpFY200"))
            operators.defineOperator(OperatorDefinition(200, OperatorType.XFX, "infixOpXFX200"))
            operators.defineOperator(OperatorDefinition(200, OperatorType.YFX, "infixOpYFX200"))
            operators.defineOperator(OperatorDefinition(500, OperatorType.XFY, "infixOpXFY500"))
            operators.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfixOpXF200"))
            operators.defineOperator(OperatorDefinition(200, OperatorType.YF, "postfixOpYF200"))

            "op priority clash 01" {
                val result = parseTerm("a infixOpXFX500 b infixOpXFX500 c")

                result.certainty shouldEqual MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "op priority clash 02" {
                val result = parseTerm("a infixOpXFX200 b postfixOpXF200")

                result.certainty shouldEqual MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "op priority clash 03" {
                val result = parseTerm("prefixOpFX200 a infixOpXFX200 b")

                result.certainty shouldEqual MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "op priority clash 04" {
                val result = parseTerm("prefixOpFX200 a postfixOpXF200.")

                result.certainty shouldEqual MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "associativity test 01 correction" {
                val result = parseTerm("a infixOpXFX200 b postfixOpYF200")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedPredicate::class)
                val outer = result.item as ParsedPredicate
                outer.name shouldEqual "postfixOpYF200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedPredicate::class)

                val inner = outer.arguments[0] as ParsedPredicate
                inner.name shouldEqual "infixOpXFX200"
                inner.arity shouldEqual 2
                inner.arguments[0] shouldEqual Atom("a")
                inner.arguments[1] shouldEqual Atom("b")
            }

            "associativity test 02" {
                val result = parseTerm("prefixOpFY200 a infixOpXFX200 b")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedPredicate::class)
                val outer = result.item as ParsedPredicate
                outer.name shouldEqual "prefixOpFY200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedPredicate::class)

                val inner = outer.arguments[0] as ParsedPredicate
                inner.name shouldEqual "infixOpXFX200"
                inner.arity shouldEqual 2
                inner.arguments[0] shouldEqual Atom("a")
                inner.arguments[1] shouldEqual Atom("b")
            }

            "associativity test 03 correction" {
                val result = parseTerm("prefixOpFX200 a infixOpYFX200 b")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedPredicate::class)
                val outer = result.item as ParsedPredicate
                outer.name shouldEqual "infixOpYFX200"
                outer.arity shouldEqual 2
                outer.arguments[0] shouldBe instanceOf(ParsedPredicate::class)
                outer.arguments[1] shouldEqual Atom("b")

                val inner = outer.arguments[0] as ParsedPredicate
                inner.name shouldEqual "prefixOpFX200"
                inner.arity shouldEqual 1
                inner.arguments[0] shouldEqual Atom("a")
            }

            "associativity test 04" {
                val result = parseTerm("prefixOpFY200 a postfixOpXF200")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedPredicate::class)
                val outer = result.item as ParsedPredicate
                outer.name shouldEqual "prefixOpFY200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedPredicate::class)

                val inner = outer.arguments[0] as ParsedPredicate
                inner.name shouldEqual "postfixOpXF200"
                inner.arity shouldEqual 1
                inner.arguments[0] shouldEqual Atom("a")
            }

            "associativity test 05 correction" {
                val result = parseTerm("prefixOpFX200 a postfixOpYF200")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedPredicate::class)
                val outer = result.item as ParsedPredicate
                outer.name shouldEqual "postfixOpYF200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedPredicate::class)

                val inner = outer.arguments[0] as ParsedPredicate
                inner.name shouldEqual "prefixOpFX200"
                inner.arity shouldEqual 1
                inner.arguments[0] shouldEqual Atom("a")
            }
        }

        "missing operand" - {
            "prefix" {
                val result = parseTerm("prefixOpFY200")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(Atom::class)
                val item = result.item as Atom
                item.name shouldEqual "prefixOpFY200"
            }

            "infix without operands falls back to atom or variable" {
                val result = parseTerm("infixOpXFX500")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(Atom::class)
                val item = result.item as Atom
                item.name shouldEqual "infixOpXFX500"
            }

            "infix before" {
                val result = parseTerm("infixOpXFX500 a")

                result.certainty shouldEqual MATCHED
                result.reportings shouldNot beEmpty()
            }

            "infix after" {
                val result = parseTerm("a infixOpXFX500")

                result.certainty shouldEqual MATCHED
                result.reportings shouldNot beEmpty()
            }

            "infix op with missing rhs operand falls back to postfix definition" {
                operators.defineOperator(OperatorDefinition(400,OperatorType.XFX,"infixAndPostfixOp"))
                operators.defineOperator(OperatorDefinition(200,OperatorType.XF,"infixAndPostfixOp"))

                val result = parseTerm("a infixAndPostfixOp")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                val item = result.item!!
                item shouldBe instanceOf(Predicate::class)
                item as Predicate
                item.name shouldEqual "infixAndPostfixOp"
                item.arity shouldEqual 1
            }

            "infix op with missing lhs operand falls back to prefix definition" {
                operators.defineOperator(OperatorDefinition(400,OperatorType.XFX,"infixAndPrefixOp"))
                operators.defineOperator(OperatorDefinition(200,OperatorType.FX,"infixAndPrefixOp"))

                val result = parseTerm("infixAndPrefixOp a")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                val item = result.item!!
                item shouldBe instanceOf(Predicate::class)
                item as Predicate
                item.name shouldEqual "infixAndPrefixOp"
                item.arity shouldEqual 1
            }
        }
    }

    "library" {
        val library = SimpleLibrary(DoublyIndexedLibraryEntryStore(), DefaultOperatorRegistry(true))
        library.include(EqualityLibrary)

        fun parseLibrary(tokens: TransactionalSequence<Token>) = PrologParser().parseLibrary(tokens, { library })
        fun parseLibrary(code: String) = parseLibrary(tokensOf(code))

        val result = parseLibrary("""
            :- op(200,xf,isDead).
            :- op(200,fx,kill).

            X isDead :- kill X, X = kenny; X = cartman.

            kill kenny.
        """)

        result.certainty shouldEqual MATCHED
        result.reportings should beEmpty()

        val rules = library.findFor(Predicate("isDead", arrayOf(Variable("X")))).toList()
        rules.size shouldBe 1

        val rule = rules.first()
        rule shouldBe instanceOf(Rule::class)
        rule as Rule

        rule.toString() shouldEqual "isDead(X) :- kill(X), =(X, kenny) ; =(X, cartman)"
    }
}}