package com.github.prologdb.parser.parser

import com.github.prologdb.parser.*
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.parser.ParseResultCertainty.MATCHED
import com.github.prologdb.parser.sequence.TransactionalSequence
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.util.DefaultOperatorRegistry
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType
import io.kotlintest.forAll
import io.kotlintest.matchers.*
import io.kotlintest.specs.FreeSpec

class PrologParserTest : FreeSpec() {
    override val oneInstancePerTest = true

    init{

    val operators = DefaultOperatorRegistry()
    operators.include(ISOOpsOperatorRegistry)
    operators.defineOperator(OperatorDefinition(500, OperatorType.XFX, "infixOpXFX500"))
    operators.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefixOpFY200"))
    operators.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefixOpFX200"))
    operators.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfixOpXF200"))

    fun tokensOf(str: String): TransactionalSequence<Token> = Lexer(SourceUnit("testcode"), str.iterator())

    fun parseTerm(tokens: TransactionalSequence<Token>) = PrologParser().parseTerm(tokens, operators, PrologParser.STOP_AT_EOF)
    fun parseTerm(code: String) = parseTerm(tokensOf(code))

    fun parseList(tokens: TransactionalSequence<Token>) = PrologParser().parseList(tokens, operators)
    fun parseList(code: String) = parseList(tokensOf(code))

    fun parseDict(tokens: TransactionalSequence<Token>) = PrologParser().parseDictionary(tokens, operators)
    fun parseDict(code: String) = parseDict(tokensOf(code))

    "atom" - {
        "a" {
            val result = parseTerm("a")
            result.certainty shouldEqual MATCHED
            result.reportings.should(beEmpty())
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

    "string" {
        val result = parseTerm(""""this is a string \" with escaped stuff"""")
        result.certainty shouldEqual MATCHED
        result.reportings should beEmpty()

        val string = result.item!! as ParsedPrologString
        string.toKotlinString() shouldEqual "this is a string \" with escaped stuff"
    }

    "simple comma separated atoms" {
        val result = parseTerm("a, b, c")
        result.certainty shouldEqual MATCHED
        result.reportings should beEmpty()

        var term = result.item!! as ParsedCompoundTerm
        term.functor shouldEqual ","
        term.arity shouldEqual 2

        (term.arguments[0] as ParsedAtom).name shouldEqual "a"

        term = term.arguments[1] as ParsedCompoundTerm
        term.functor shouldEqual ","
        term.arity shouldEqual  2

        (term.arguments[0] as ParsedAtom).name shouldEqual "b"
        (term.arguments[1] as ParsedAtom).name shouldEqual "c"
    }
    
    "compound term" - {
        "without arguments" {
            val result = parseTerm("predicate()")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()

            val compound = result.item!! as CompoundTerm
            compound.functor shouldEqual "predicate"
            compound.arity shouldEqual 0
        }

        "with three arguments" {
            val result = parseTerm("predicate(foo, X, bar)")
            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            val compound = result.item!! as CompoundTerm
            compound.functor shouldEqual "predicate"
            compound.arguments.size shouldEqual 3

            assert(compound.arguments[0] is Atom)
            (compound.arguments[0] as Atom).name shouldEqual "foo"

            assert(compound.arguments[1] is Variable)
            (compound.arguments[1] as Variable).name shouldEqual "X"

            assert(compound.arguments[2] is Atom)
            (compound.arguments[2] as Atom).name shouldEqual "bar"
        }

        "missing closing parenthesis: EOF instead" {
            val result = parseTerm("predicate(foo, X")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 1
        }

        "missing closing parenthesis: . instead" {
            val result = parseTerm("predicate(foo, X.")
            result.reportings shouldNot beEmpty()
        }

        "infix" {
            val result = parseTerm("a infixOpXFX500 b")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val compound = result.item!! as ParsedCompoundTerm
            compound.functor shouldEqual "infixOpXFX500"
            compound.arguments.size shouldEqual 2
        }

        "invocation with infix as sole parameter" {
            val result = parseTerm("a(b infixOpXFX500 c)")
            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val compound = result.item!! as ParsedCompoundTerm
            compound.functor shouldEqual "a"
            compound.arity shouldEqual 1

            val soleArg = compound.arguments[0] as ParsedCompoundTerm
            soleArg.functor shouldEqual "infixOpXFX500"
            soleArg.arity shouldEqual 2
            soleArg.arguments[0].toString() shouldEqual "b"
            soleArg.arguments[1].toString() shouldEqual "c"
        }

        "infix with prefix as a parameter" {
            val result = parseTerm("prefixOpFY200 a infixOpXFX500 b")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as CompoundTerm
            item.functor shouldEqual "infixOpXFX500"
            item.arity shouldEqual 2

            val lhs = item.arguments[0] as CompoundTerm
            lhs.functor shouldEqual "prefixOpFY200"
            lhs.arity shouldEqual 1
        }

        "prefix op" {
            val result = parseTerm("prefixOpFY200 b")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as CompoundTerm
            item.functor shouldEqual "prefixOpFY200"
            item.arity shouldEqual 1
        }

        "rule head with postfix in head" {
            val result = parseTerm("a postfixOpXF200 infixOpXFX500 b")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as CompoundTerm
            item.functor shouldEqual "infixOpXFX500"
            item.arity shouldEqual 2

            val lhs = item.arguments[0] as CompoundTerm
            lhs.functor shouldEqual "postfixOpXF200"
            lhs.arity shouldEqual 1

            val rhs = item.arguments[1] as Atom
            rhs.name shouldEqual "b"
        }

        "prefix plus infix" {
            val result = parseTerm("prefixOpFX200 a infixOpXFX500 b")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            val item = result.item!! as CompoundTerm
            item.functor shouldEqual "infixOpXFX500"
            item.arity shouldBe 2

            val lhs = item.arguments[0] as CompoundTerm
            lhs.functor shouldEqual "prefixOpFX200"
            lhs.arity shouldEqual 1
            (lhs.arguments[0] as Atom).name shouldEqual "a"

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

    "dict" - {
        "{}" {
            val result = parseDict("{}")

            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedDictionary)
            result.item!!.pairs.entries should beEmpty()
            result.item!!.tail shouldBe null
        }

        "{a:1}" {
            val result = parseDict("{a:1}")

            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedDictionary)
            result.item!!.pairs.size shouldEqual 1
            result.item!!.tail shouldBe null

            val pair = result.item!!.pairs.entries.first()
            pair.key shouldEqual Atom("a")
            pair.value shouldEqual PrologInteger(1)
        }

        "{a : 1, b : 2}" {
            val result = parseDict("{a : 1, b : 2}")

            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedDictionary)
            result.item!!.pairs.size shouldEqual 2
            result.item!!.tail shouldBe null

            result.item!!.pairs[Atom("a")]!! shouldEqual PrologInteger(1)
            result.item!!.pairs[Atom("b")]!! shouldEqual PrologInteger(2)
        }

        "{a : 1, b : 2 | T}" {
            val result = parseDict("{a : 1, b : 2 | T}")

            result.certainty shouldEqual MATCHED
            result.reportings should beEmpty()
            assert(result.item is ParsedDictionary)
            result.item!!.pairs.size shouldEqual 2
            result.item!!.tail shouldEqual Variable("T")

            result.item!!.pairs[Atom("a")]!! shouldEqual PrologInteger(1)
            result.item!!.pairs[Atom("b")]!! shouldEqual PrologInteger(2)
        }

        "{a}" {
            val result = parseDict("{a}")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldBe 1
            assert(result.item is ParsedDictionary)
            result.item!!.pairs.entries should beEmpty()
            result.item!!.tail shouldBe null

            result.reportings.first() as SyntaxError
        }

        "{T:1}" {
            val result = parseDict("{T:1}")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldBe 1
            assert(result.item is ParsedDictionary)
            result.item!!.pairs.entries should beEmpty()
            result.item!!.tail shouldBe null

            result.reportings.first() as SyntaxError
        }

        "{a:1, a:2, b:3, b:4}" {
            val result = parseDict("{a:1, a:2, b:3, b:4}")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldEqual 2
            assert(result.item is ParsedDictionary)
            result.item!!.pairs.size shouldEqual 2
            result.item!!.tail shouldBe null

            forAll(result.reportings) {
                it shouldBe instanceOf(SemanticWarning::class)
            }
        }
    }

    "expression term" - {
        "double prefix" {
            val result = parseTerm("prefixOpFY200 prefixOpFY200 a")

            result.certainty shouldEqual MATCHED
            result.reportings.size shouldEqual 0

            var item = result.item!! as CompoundTerm
            item.functor shouldEqual "prefixOpFY200"
            item.arity shouldEqual 1

            item = item.arguments[0] as CompoundTerm
            item.functor shouldEqual "prefixOpFY200"
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
                val result = parseTerm("prefixOpFX200 a postfixOpXF200")

                result.certainty shouldEqual MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "associativity test 01 correction" {
                val result = parseTerm("a infixOpXFX200 b postfixOpYF200")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedCompoundTerm::class)
                val outer = result.item as ParsedCompoundTerm
                outer.functor shouldEqual "postfixOpYF200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedCompoundTerm::class)

                val inner = outer.arguments[0] as ParsedCompoundTerm
                inner.functor shouldEqual "infixOpXFX200"
                inner.arity shouldEqual 2
                inner.arguments[0] shouldEqual Atom("a")
                inner.arguments[1] shouldEqual Atom("b")
            }

            "associativity test 02" {
                val result = parseTerm("prefixOpFY200 a infixOpXFX200 b")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedCompoundTerm::class)
                val outer = result.item as ParsedCompoundTerm
                outer.functor shouldEqual "prefixOpFY200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedCompoundTerm::class)

                val inner = outer.arguments[0] as ParsedCompoundTerm
                inner.functor shouldEqual "infixOpXFX200"
                inner.arity shouldEqual 2
                inner.arguments[0] shouldEqual Atom("a")
                inner.arguments[1] shouldEqual Atom("b")
            }

            "associativity test 03 correction" {
                val result = parseTerm("prefixOpFX200 a infixOpYFX200 b")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedCompoundTerm::class)
                val outer = result.item as ParsedCompoundTerm
                outer.functor shouldEqual "infixOpYFX200"
                outer.arity shouldEqual 2
                outer.arguments[0] shouldBe instanceOf(ParsedCompoundTerm::class)
                outer.arguments[1] shouldEqual Atom("b")

                val inner = outer.arguments[0] as ParsedCompoundTerm
                inner.functor shouldEqual "prefixOpFX200"
                inner.arity shouldEqual 1
                inner.arguments[0] shouldEqual Atom("a")
            }

            "associativity test 04" {
                val result = parseTerm("prefixOpFY200 a postfixOpXF200")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedCompoundTerm::class)
                val outer = result.item as ParsedCompoundTerm
                outer.functor shouldEqual "prefixOpFY200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedCompoundTerm::class)

                val inner = outer.arguments[0] as ParsedCompoundTerm
                inner.functor shouldEqual "postfixOpXF200"
                inner.arity shouldEqual 1
                inner.arguments[0] shouldEqual Atom("a")
            }

            "associativity test 05 correction" {
                val result = parseTerm("prefixOpFX200 a postfixOpYF200")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(ParsedCompoundTerm::class)
                val outer = result.item as ParsedCompoundTerm
                outer.functor shouldEqual "postfixOpYF200"
                outer.arity shouldEqual 1
                outer.arguments[0] shouldBe instanceOf(ParsedCompoundTerm::class)

                val inner = outer.arguments[0] as ParsedCompoundTerm
                inner.functor shouldEqual "prefixOpFX200"
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
                operators.defineOperator(OperatorDefinition(400, OperatorType.XFX, "infixAndPostfixOp"))
                operators.defineOperator(OperatorDefinition(200, OperatorType.XF, "infixAndPostfixOp"))

                val result = parseTerm("a infixAndPostfixOp")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                val item = result.item!!
                item shouldBe instanceOf(CompoundTerm::class)
                item as CompoundTerm
                item.functor shouldEqual "infixAndPostfixOp"
                item.arity shouldEqual 1
            }

            "infix op with missing lhs operand falls back to prefix definition" {
                operators.defineOperator(OperatorDefinition(400, OperatorType.XFX, "infixAndPrefixOp"))
                operators.defineOperator(OperatorDefinition(200, OperatorType.FX, "infixAndPrefixOp"))

                val result = parseTerm("infixAndPrefixOp a")

                result.certainty shouldEqual MATCHED
                result.reportings should beEmpty()

                val item = result.item!!
                item shouldBe instanceOf(CompoundTerm::class)
                item as CompoundTerm
                item.functor shouldEqual "infixAndPrefixOp"
                item.arity shouldEqual 1
            }
        }
    }

    "parenthesis protection" {
        val tokens = tokensOf("a((b(1), c(1)))")
        val result = parseTerm(tokens)
        result.certainty shouldEqual MATCHED
        result.reportings should beEmpty()
        val compound = result.item as CompoundTerm
        compound.arguments.size shouldBe 1
        val arg0 = compound.arguments[0] as CompoundTerm
        arg0.functor shouldBe ","
        arg0.arity shouldBe 2

        shouldThrow<IllegalStateException> {
            tokens.commit()
        }
    }

    "module" {
        fun parseModule(tokens: TransactionalSequence<Token>) = PrologParser().parseModule(tokens, ISOOpsOperatorRegistry)
        fun parseModule(code: String) = parseModule(tokensOf(code))

        val result = parseModule("""
            :- module(test).

            :- op(200,xf,isDead).
            :- op(200,fx,kill).

            X isDead :- kill X, X = kenny; X = cartman.

            kill kenny.
        """)

        result.certainty shouldEqual MATCHED
        result.reportings should beEmpty()

        val module = result.item!!
        module.name shouldBe "test"

        module.exportedPredicates.size shouldBe 2
        module.exportedPredicates should haveKey(ClauseIndicator.of("isDead", 1))
        module.exportedPredicates should haveKey(ClauseIndicator.of("kill", 1))

        val isDead1 = module.exportedPredicates[ClauseIndicator.of("isDead", 1)]!!
        isDead1 shouldBe instanceOf(ASTPrologPredicate::class)
        isDead1 as ASTPrologPredicate
        isDead1.functor shouldBe "isDead"
        isDead1.arity shouldBe 1
        isDead1.clauses should haveSize(1)
        isDead1.clauses[0] shouldBe instanceOf(Rule::class)

        isDead1.clauses[0].toString() shouldEqual "isDead(X) :- kill(X), =(X, kenny) ; =(X, cartman)"
    }
}}
