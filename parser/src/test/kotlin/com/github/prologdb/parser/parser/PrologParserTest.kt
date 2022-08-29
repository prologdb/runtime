package com.github.prologdb.parser.parser

import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticWarning
import com.github.prologdb.parser.SyntaxError
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.Token
import com.github.prologdb.parser.parser.ParseResultCertainty.MATCHED
import com.github.prologdb.parser.sequence.TransactionalSequence
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.parser.withMockSourceLocation
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.util.DefaultOperatorRegistry
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.IsolationMode
import io.kotest.core.spec.style.FreeSpec
import io.kotest.inspectors.forAll
import io.kotest.inspectors.forOne
import io.kotest.matchers.collections.beEmpty
import io.kotest.matchers.collections.haveSize
import io.kotest.matchers.maps.haveKey
import io.kotest.matchers.should
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNot
import io.kotest.matchers.shouldNotBe
import io.kotest.matchers.string.contain
import io.kotest.matchers.types.beInstanceOf
import io.kotest.matchers.types.instanceOf

class PrologParserTest : FreeSpec({
    val operators = DefaultOperatorRegistry().apply {
        include(ISOOpsOperatorRegistry)
    }
    operators.defineOperator(OperatorDefinition(500, OperatorType.XFX, "infixOpXFX500"))
    operators.defineOperator(OperatorDefinition(200, OperatorType.FY, "prefixOpFY200"))
    operators.defineOperator(OperatorDefinition(200, OperatorType.FX, "prefixOpFX200"))
    operators.defineOperator(OperatorDefinition(200, OperatorType.XF, "postfixOpXF200"))

    fun tokensOf(str: String): TransactionalSequence<Token> = Lexer(SourceUnit("testcode"), str.iterator())

    fun parseTerm(tokens: TransactionalSequence<Token>) = PrologParser().parseTerm(tokens, operators, StopCondition.STOP_AT_EOF)
    fun parseTerm(code: String) = parseTerm(tokensOf(code))

    fun parseList(tokens: TransactionalSequence<Token>) = PrologParser().parseList(tokens, operators)
    fun parseList(code: String) = parseList(tokensOf(code))

    fun parseDict(tokens: TransactionalSequence<Token>) = PrologParser().parseDictionary(tokens, operators)
    fun parseDict(code: String) = parseDict(tokensOf(code))

    fun parseQuery(tokens: TransactionalSequence<Token>) = PrologParser().parseQuery(tokens, operators)
    fun parseQuery(code: String) = parseQuery(tokensOf(code))

    "atom" - {
        "a" {
            val result = parseTerm("a")
            result.certainty shouldBe MATCHED
            result.reportings.should(beEmpty())
            assert(result.item is Atom)
            (result.item!! as Atom).name shouldBe "a"
        }

        "someAtom" {
            val result = parseTerm("someAtom")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is Atom)
            (result.item!! as Atom).name shouldBe "someAtom"
        }
    }

    "variable" - {
        "X" {
            val result = parseTerm("X")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldBe "X"
        }

        "Variable" {
            val result = parseTerm("Variable")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldBe "Variable"
        }

        "_underscore" {
            val result = parseTerm("_underscore")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is Variable)
            (result.item!! as Variable).name shouldBe "_underscore"
        }
    }

    "string" {
        val result = parseTerm(""""this is a string \" with escaped stuff"""")
        result.certainty shouldBe MATCHED
        result.reportings should beEmpty()

        val string = result.item!! as PrologString
        string.toKotlinString() shouldBe "this is a string \" with escaped stuff"
    }

    "simple comma separated atoms" {
        val result = parseTerm("a, b, c")
        result.certainty shouldBe MATCHED
        result.reportings should beEmpty()

        var term = result.item!! as CompoundTerm
        term.functor shouldBe ","
        term.arity shouldBe 2

        (term.arguments[0] as Atom).name shouldBe "a"

        term = term.arguments[1] as CompoundTerm
        term.functor shouldBe ","
        term.arity shouldBe  2

        (term.arguments[0] as Atom).name shouldBe "b"
        (term.arguments[1] as Atom).name shouldBe "c"
    }

    "compound term" - {
        "without arguments" {
            val result = parseTerm("predicate()")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()

            val compound = result.item!! as CompoundTerm
            compound.functor shouldBe "predicate"
            compound.arity shouldBe 0
        }

        "with three arguments" {
            val result = parseTerm("predicate(foo, X, bar)")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            val compound = result.item!! as CompoundTerm
            compound.functor shouldBe "predicate"
            compound.arguments.size shouldBe 3

            assert(compound.arguments[0] is Atom)
            (compound.arguments[0] as Atom).name shouldBe "foo"

            assert(compound.arguments[1] is Variable)
            (compound.arguments[1] as Variable).name shouldBe "X"

            assert(compound.arguments[2] is Atom)
            (compound.arguments[2] as Atom).name shouldBe "bar"
        }

        "missing closing parenthesis: EOF instead" {
            val result = parseTerm("predicate(foo, X")
            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 1
        }

        "missing closing parenthesis: . instead" {
            val result = parseTerm("predicate(foo, X.")
            result.reportings shouldNot beEmpty()
        }

        "infix" {
            val result = parseTerm("a infixOpXFX500 b")
            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 0

            val compound = result.item!! as CompoundTerm
            compound.functor shouldBe "infixOpXFX500"
            compound.arguments.size shouldBe 2
        }

        "invocation with infix as sole parameter" {
            val result = parseTerm("a(b infixOpXFX500 c)")
            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 0

            val compound = result.item!! as CompoundTerm
            compound.functor shouldBe "a"
            compound.arity shouldBe 1

            val soleArg = compound.arguments[0] as CompoundTerm
            soleArg.functor shouldBe "infixOpXFX500"
            soleArg.arity shouldBe 2
            soleArg.arguments[0].toString() shouldBe "b"
            soleArg.arguments[1].toString() shouldBe "c"
        }

        "infix with prefix as a parameter" {
            val result = parseTerm("prefixOpFY200 a infixOpXFX500 b")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 0

            val item = result.item!! as CompoundTerm
            item.functor shouldBe "infixOpXFX500"
            item.arity shouldBe 2

            val lhs = item.arguments[0] as CompoundTerm
            lhs.functor shouldBe "prefixOpFY200"
            lhs.arity shouldBe 1
        }

        "prefix op" {
            val result = parseTerm("prefixOpFY200 b")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 0

            val item = result.item!! as CompoundTerm
            item.functor shouldBe "prefixOpFY200"
            item.arity shouldBe 1
        }

        "rule head with postfix in head" {
            val result = parseTerm("a postfixOpXF200 infixOpXFX500 b")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 0

            val item = result.item!! as CompoundTerm
            item.functor shouldBe "infixOpXFX500"
            item.arity shouldBe 2

            val lhs = item.arguments[0] as CompoundTerm
            lhs.functor shouldBe "postfixOpXF200"
            lhs.arity shouldBe 1

            val rhs = item.arguments[1] as Atom
            rhs.name shouldBe "b"
        }

        "prefix plus infix" {
            val result = parseTerm("prefixOpFX200 a infixOpXFX500 b")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 0

            val item = result.item!! as CompoundTerm
            item.functor shouldBe "infixOpXFX500"
            item.arity shouldBe 2

            val lhs = item.arguments[0] as CompoundTerm
            lhs.functor shouldBe "prefixOpFX200"
            lhs.arity shouldBe 1
            (lhs.arguments[0] as Atom).name shouldBe "a"

            val rhs = item.arguments[1] as Atom
            rhs.name shouldBe "b"
        }
    }

    "list" - {
        "[]" {
            val tokens = tokensOf("[]")
            val result = parseList(tokens)
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 0
            result.item!!.tail shouldBe null

            shouldThrow<IllegalStateException> {
                tokens.commit()
            }
        }

        "[1,2]" {
            val result = parseList(tokensOf("[1,2]"))
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 2
            result.item!!.tail shouldBe null
        }

        "[1|T]" {
            val result = parseList("[1|T]")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 1
            assert(result.item!!.tail is Variable)
            (result.item!!.tail as Variable).name shouldBe "T"
        }

        "[1,2|T]" {
            val result = parseList("[1,2|T]")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 2
            assert(result.item!!.tail is Variable)
            (result.item!!.tail as Variable).name shouldBe "T"
        }

        "invalid: [1," {
            val result = parseList("[1,")
            result.reportings shouldNot beEmpty()
        }

        "invalid: [1,]" {
            val result = parseList("[1,]")
            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 1
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 1
            result.item!!.tail shouldBe null
        }

        "invalid: [1|]" {
            val result = parseList("[1|]")
            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 1
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 1
            result.item!!.tail shouldBe null
        }

        "[1,2|[3,4]]" {
            val result = parseList("[1,2|[3,4]]")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 4
            result.item!!.tail shouldBe null
        }

        "[1,2|[3|T]]" {
            val result = parseList("[1,2|[3|T]]")
            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologList)
            result.item!!.elements.size shouldBe 3
            assert(result.item!!.tail is Variable)
            (result.item!!.tail as Variable).name shouldBe "T"
        }
    }

    "dict" - {
        "{}" {
            val result = parseDict("{}")

            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologDictionary)
            result.item!!.pairs.entries should beEmpty()
            result.item!!.tail shouldBe null
        }

        "{a:1}" {
            val result = parseDict("{a:1}")

            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologDictionary)
            result.item!!.pairs.size shouldBe 1
            result.item!!.tail shouldBe null

            val pair = result.item!!.pairs.entries.first()
            pair.key shouldBe Atom("a")
            pair.value shouldBe PrologNumber(1)
        }

        "{a : 1, b : 2}" {
            val result = parseDict("{a : 1, b : 2}")

            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologDictionary)
            result.item!!.pairs.size shouldBe 2
            result.item!!.tail shouldBe null

            result.item!!.pairs[Atom("a")]!! shouldBe PrologNumber(1)
            result.item!!.pairs[Atom("b")]!! shouldBe PrologNumber(2)
        }

        "{a : 1, b : 2 | T}" {
            val result = parseDict("{a : 1, b : 2 | T}")

            result.certainty shouldBe MATCHED
            result.reportings should beEmpty()
            assert(result.item is PrologDictionary)
            result.item!!.pairs.size shouldBe 2
            result.item!!.tail shouldBe Variable("T")

            result.item!!.pairs[Atom("a")]!! shouldBe PrologNumber(1)
            result.item!!.pairs[Atom("b")]!! shouldBe PrologNumber(2)
        }

        "{a}" {
            val result = parseDict("{a}")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 1
            assert(result.item is PrologDictionary)
            result.item!!.pairs.entries should beEmpty()
            result.item!!.tail shouldBe null

            result.reportings.first() as SyntaxError
        }

        "{T:1}" {
            val result = parseDict("{T:1}")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 1
            assert(result.item is PrologDictionary)
            result.item!!.pairs.entries should beEmpty()
            result.item!!.tail shouldBe null

            result.reportings.first() as SyntaxError
        }

        "{a:1, a:2, b:3, b:4}" {
            val result = parseDict("{a:1, a:2, b:3, b:4}")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 2
            assert(result.item is PrologDictionary)
            result.item!!.pairs.size shouldBe 2
            result.item!!.tail shouldBe null

            result.reportings.forAll {
                it shouldBe instanceOf(SemanticWarning::class)
            }
        }
    }

    "expression term" - {
        "double prefix" {
            val result = parseTerm("prefixOpFY200 prefixOpFY200 a")

            result.certainty shouldBe MATCHED
            result.reportings.size shouldBe 0

            var item = result.item!! as CompoundTerm
            item.functor shouldBe "prefixOpFY200"
            item.arity shouldBe 1

            item = item.arguments[0] as CompoundTerm
            item.functor shouldBe "prefixOpFY200"
            item.arity shouldBe 1
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

                result.certainty shouldBe MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "op priority clash 02" {
                val result = parseTerm("a infixOpXFX200 b postfixOpXF200")

                result.certainty shouldBe MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "op priority clash 03" {
                val result = parseTerm("prefixOpFX200 a infixOpXFX200 b")

                result.certainty shouldBe MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "op priority clash 04" {
                val result = parseTerm("prefixOpFX200 a postfixOpXF200")

                result.certainty shouldBe MATCHED
                result.reportings shouldNot beEmpty()
                result.item shouldNotBe null
            }

            "associativity test 01 correction" {
                val result = parseTerm("a infixOpXFX200 b postfixOpYF200")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(CompoundTerm::class)
                val outer = result.item as CompoundTerm
                outer.functor shouldBe "postfixOpYF200"
                outer.arity shouldBe 1
                outer.arguments[0] shouldBe instanceOf(CompoundTerm::class)

                val inner = outer.arguments[0] as CompoundTerm
                inner.functor shouldBe "infixOpXFX200"
                inner.arity shouldBe 2
                inner.arguments[0] shouldBe Atom("a")
                inner.arguments[1] shouldBe Atom("b")
            }

            "associativity test 02" {
                val result = parseTerm("prefixOpFY200 a infixOpXFX200 b")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(CompoundTerm::class)
                val outer = result.item as CompoundTerm
                outer.functor shouldBe "prefixOpFY200"
                outer.arity shouldBe 1
                outer.arguments[0] shouldBe instanceOf(CompoundTerm::class)

                val inner = outer.arguments[0] as CompoundTerm
                inner.functor shouldBe "infixOpXFX200"
                inner.arity shouldBe 2
                inner.arguments[0] shouldBe Atom("a")
                inner.arguments[1] shouldBe Atom("b")
            }

            "associativity test 03 correction" {
                val result = parseTerm("prefixOpFX200 a infixOpYFX200 b")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(CompoundTerm::class)
                val outer = result.item as CompoundTerm
                outer.functor shouldBe "infixOpYFX200"
                outer.arity shouldBe 2
                outer.arguments[0] shouldBe instanceOf(CompoundTerm::class)
                outer.arguments[1] shouldBe Atom("b")

                val inner = outer.arguments[0] as CompoundTerm
                inner.functor shouldBe "prefixOpFX200"
                inner.arity shouldBe 1
                inner.arguments[0] shouldBe Atom("a")
            }

            "associativity test 04" {
                val result = parseTerm("prefixOpFY200 a postfixOpXF200")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(CompoundTerm::class)
                val outer = result.item as CompoundTerm
                outer.functor shouldBe "prefixOpFY200"
                outer.arity shouldBe 1
                outer.arguments[0] shouldBe instanceOf(CompoundTerm::class)

                val inner = outer.arguments[0] as CompoundTerm
                inner.functor shouldBe "postfixOpXF200"
                inner.arity shouldBe 1
                inner.arguments[0] shouldBe Atom("a")
            }

            "associativity test 05 correction" {
                val result = parseTerm("prefixOpFX200 a postfixOpYF200")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(CompoundTerm::class)
                val outer = result.item as CompoundTerm
                outer.functor shouldBe "postfixOpYF200"
                outer.arity shouldBe 1
                outer.arguments[0] shouldBe instanceOf(CompoundTerm::class)

                val inner = outer.arguments[0] as CompoundTerm
                inner.functor shouldBe "prefixOpFX200"
                inner.arity shouldBe 1
                inner.arguments[0] shouldBe Atom("a")
            }

            "associativity test 06 correction" {
                val result = parseTerm("dynamic append/3")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(CompoundTerm::class)

                val outerDynamic = result.item as CompoundTerm
                outerDynamic.functor shouldBe "dynamic"
                outerDynamic.arity shouldBe 1
                outerDynamic.arguments[0] should beInstanceOf(CompoundTerm::class)

                val indicator = outerDynamic.arguments[0] as CompoundTerm
                indicator.functor shouldBe "/"
                indicator.arity shouldBe 2
                indicator.arguments[0] shouldBe Atom("append")
                indicator.arguments[1] shouldBe PrologNumber(3)
            }
        }

        "missing operand" - {
            "prefix" {
                val result = parseTerm("prefixOpFY200")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(Atom::class)
                val item = result.item as Atom
                item.name shouldBe "prefixOpFY200"
            }

            "infix without operands falls back to atom or variable" {
                val result = parseTerm("infixOpXFX500")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                result.item shouldBe instanceOf(Atom::class)
                val item = result.item as Atom
                item.name shouldBe "infixOpXFX500"
            }

            "infix before" {
                val result = parseTerm("infixOpXFX500 a")

                result.certainty shouldBe MATCHED
                result.reportings shouldNot beEmpty()
            }

            "infix after" {
                val result = parseTerm("a infixOpXFX500")

                result.certainty shouldBe MATCHED
                result.reportings shouldNot beEmpty()
            }

            "infix op with missing rhs operand falls back to postfix definition" {
                operators.defineOperator(OperatorDefinition(400, OperatorType.XFX, "infixAndPostfixOp"))
                operators.defineOperator(OperatorDefinition(200, OperatorType.XF, "infixAndPostfixOp"))

                val result = parseTerm("a infixAndPostfixOp")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                val item = result.item!!
                item shouldBe instanceOf(CompoundTerm::class)
                item as CompoundTerm
                item.functor shouldBe "infixAndPostfixOp"
                item.arity shouldBe 1
            }

            "infix op with missing lhs operand falls back to prefix definition" {
                operators.defineOperator(OperatorDefinition(400, OperatorType.XFX, "infixAndPrefixOp"))
                operators.defineOperator(OperatorDefinition(200, OperatorType.FX, "infixAndPrefixOp"))

                val result = parseTerm("infixAndPrefixOp a")

                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()

                val item = result.item!!
                item shouldBe instanceOf(CompoundTerm::class)
                item as CompoundTerm
                item.functor shouldBe "infixAndPrefixOp"
                item.arity shouldBe 1
            }
        }
    }

    "invalid :-op/3" - {
        "non-numeric precedence" {
            val declaration = CompoundTerm("op", arrayOf(Atom("invalid"), Atom("xfx"), Atom("opname"))).withMockSourceLocation()
            val result = PrologParser().parseOperatorDefinition(declaration)
            result.item shouldBe null
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "operator precedence must be an integer"
        }

        "precedence out of range below" {
            val declaration = CompoundTerm("op", arrayOf(PrologNumber(-10), Atom("xfx"), Atom("opname"))).withMockSourceLocation()
            val result = PrologParser().parseOperatorDefinition(declaration)
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "operator precedence must be between 0 and 1200 (inclusive)"
            result.item shouldNotBe null
            result.item!!.precedence shouldBe 0.toShort()
        }

        "precedence out of range above" {
            val declaration = CompoundTerm("op", arrayOf(PrologNumber(1201), Atom("xfx"), Atom("opname"))).withMockSourceLocation()
            val result = PrologParser().parseOperatorDefinition(declaration)
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "operator precedence must be between 0 and 1200 (inclusive)"
            result.item shouldNotBe null
            result.item!!.precedence shouldBe 1200.toShort()
        }

        "type argument not atom" {
            val declaration = CompoundTerm("op", arrayOf(PrologNumber(400), PrologList(emptyList()), Atom("opname"))).withMockSourceLocation()
            val result = PrologParser().parseOperatorDefinition(declaration)
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "operator type: expected atom but got list"
            result.item shouldBe null
        }

        "undefined type" {
            val declaration = CompoundTerm("op", arrayOf(PrologNumber(400), Atom("yfy"), Atom("opname"))).withMockSourceLocation()
            val result = PrologParser().parseOperatorDefinition(declaration)
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "yfy is not a known operator type"
            result.item shouldBe null
        }

        "name argument not atom" {
            val declaration = CompoundTerm("op", arrayOf(PrologNumber(400), Atom("xfx"), PrologList(emptyList()))).withMockSourceLocation()
            val result = PrologParser().parseOperatorDefinition(declaration)
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "operator name: expected atom but got list"
            result.item shouldBe null
        }
    }

    "valid :-op/3" {
        val declaration = CompoundTerm("op", arrayOf(PrologNumber(800), Atom("xfx"), Atom("myop"))).withMockSourceLocation()
        val result = PrologParser().parseOperatorDefinition(declaration)
        result.reportings should beEmpty()
        result.item shouldNotBe null
        result.item!!.precedence shouldBe 800.toShort()
        result.item!!.type shouldBe OperatorType.XFX
        result.item!!.name shouldBe "myop"
    }

    "invalid :-module/1" {
        val declaration = CompoundTerm("module", arrayOf(PrologList(emptyList()))).withMockSourceLocation()
        val result = PrologParser().parseModuleDeclaration(declaration)
        result.reportings.size shouldBe 1
        result.reportings.first().message shouldBe "Argument 0 to module/1 must be an atom, got list"
        result.item shouldBe null
    }

    "valid :-module/1" {
        val declaration = CompoundTerm("module", arrayOf(Atom("mymodule"))).withMockSourceLocation()
        val result = PrologParser().parseModuleDeclaration(declaration)
        result.reportings should beEmpty()
        result.item shouldNotBe null
        result.item!!.moduleName shouldBe "mymodule"
        result.item!!.exportedPredicates shouldBe null
    }

    "invalid :-module/2" - {
        "second argument not a list" {
            val declaration = CompoundTerm("module", arrayOf(Atom("name"), Atom("bla"))).withMockSourceLocation()
            val result = PrologParser().parseModuleDeclaration(declaration)
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "Argument 1 to module/2 must be a list, got atom"
            result.item shouldNotBe null
            result.item!!.moduleName shouldBe "name"
            result.item!!.exportedPredicates shouldBe null
        }
        "export list with invalid clause indicator" {
            val declaration = CompoundTerm("module", arrayOf(Atom("name"), PrologList(listOf(Atom("predname"))))).withMockSourceLocation()
            val result = PrologParser().parseModuleDeclaration(declaration)
            result.reportings.size shouldBe 1
            result.reportings.first().message shouldBe "Module exports must be instances of '/'/2 or op/3, got atom"
            result.item shouldNotBe null
            result.item!!.moduleName shouldBe "name"
            result.item!!.exportedPredicates shouldNotBe null
            result.item!!.exportedPredicates!! should beEmpty()
        }
    }

    "valid :-module/2" {
        val exports = listOf(
            CompoundTerm("/", arrayOf(Atom("predname"), PrologNumber(2))),
            CompoundTerm("/", arrayOf(Atom("otherpred"), PrologNumber(4)))
        )
        val declaration = CompoundTerm("module", arrayOf(Atom("name"), PrologList(exports))).withMockSourceLocation()
        val result = PrologParser().parseModuleDeclaration(declaration)
        result.reportings should beEmpty()
        result.item shouldNotBe null
        result.item!!.moduleName shouldBe "name"
        result.item!!.exportedPredicates shouldNotBe null
        result.item!!.exportedPredicates!!.size shouldBe 2

        result.item!!.exportedPredicates!!.forOne {
            it.functor shouldBe "predname"
            it.arity shouldBe 2
        }

        result.item!!.exportedPredicates!!.forOne {
            it.functor shouldBe "otherpred"
            it.arity shouldBe 4
        }
    }

    "parenthesis protection" {
        val tokens = tokensOf("a((b(1), c(1)))")
        val result = parseTerm(tokens)
        result.certainty shouldBe MATCHED
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

    "module qualified goal" {
        val tokens = tokensOf("lists:(append()).")
        val result = parseQuery(tokens)
        result.certainty shouldBe MATCHED
        result.reportings should beEmpty()

        result.item!! should beInstanceOf(PredicateInvocationQuery::class)

        val outerColon = (result.item!! as PredicateInvocationQuery).goal
        outerColon.functor shouldBe ":"
        outerColon.arity shouldBe 2
        outerColon.arguments[0] should beInstanceOf(Atom::class)
        (outerColon.arguments[0] as Atom).name shouldBe "lists"
        outerColon.arguments[1] should beInstanceOf(CompoundTerm::class)

        val innerGoal = outerColon.arguments[1] as CompoundTerm
        innerGoal.functor shouldBe "append"
        innerGoal.arity shouldBe 0
    }

    "module" {
        fun parseModule(tokens: TransactionalSequence<Token>): ParseResult<com.github.prologdb.runtime.module.Module> {
            val runtime = DefaultPrologRuntimeEnvironment()
            val primedStage = PrologParser().parseSourceFile(
                tokens,
                DefaultModuleSourceFileVisitor(runtime, emptySet()),
            )
            val parsedStage = primedStage.proceed()

            return ParseResult(parsedStage.module, MATCHED, parsedStage.reportings)
        }
        fun parseModule(code: String) = parseModule(tokensOf(code))

        val result = parseModule("""
            :- module(test).
    
            :- op(200,xf,isDead).
            :- op(200,fx,kill).
    
            X isDead :- kill X, X = kenny; X = cartman.
    
            kill kenny.
        """)

        result.certainty shouldBe MATCHED
        result.reportings should beEmpty()

        val module = result.item!!
        module.declaration.moduleName shouldBe "test"

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

        isDead1.clauses[0].toString() shouldBe "isDead(X) :- kill(X), =(X, kenny) ; =(X, cartman)"
    }

    "query" - {
        "content after full stop" {
            val result = parseQuery("some_goal(). other().")
            result.item shouldNotBe null
            result.isSuccess shouldBe true
            result.item!! should beInstanceOf(PredicateInvocationQuery::class)
            result.item!! as PredicateInvocationQuery
            (result.item!! as PredicateInvocationQuery).goal shouldBe CompoundTerm("some_goal", emptyArray())

            result.reportings should haveSize(1)
            result.reportings.single().location.line shouldBe 1
            result.reportings.single().location.column shouldBe 13
        }
        "missing full stop at end of query" {
            val result = parseQuery("some_goal(Var)")
            result.certainty shouldBe MATCHED
            result.reportings should haveSize(1)
            result.reportings.single().level shouldBe Reporting.Level.ERROR
            result.reportings.single().message should contain("missing operator .")
            result.item shouldBe null
        }

        "missing closing parenthesis and full stop and end of query" {
            val result = parseQuery("some_goal(Var")
            result.certainty shouldBe MATCHED
            result.reportings should haveSize(1)
            result.reportings.single().level shouldBe Reporting.Level.ERROR
            result.reportings.single().message should contain("missing operator )")
            result.item shouldBe null
        }

        "missing closing parenthesis but not full stop and end of query" {
            val result = parseQuery("some_goal(Var.")
            result.certainty shouldBe MATCHED
            result.reportings should haveSize(1)
            result.reportings.single().level shouldBe Reporting.Level.ERROR
            result.reportings.single().message should contain("missing operator )")
            result.item shouldBe null
        }

        "zero arity goal without parenthesis" - {
            "invocation with identifier token" {
                val result = parseQuery("true.")
                result.isSuccess shouldBe true
                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()
                result.item shouldNotBe null
                result.item!! should beInstanceOf<PredicateInvocationQuery>()
                (result.item!! as PredicateInvocationQuery).goal shouldBe CompoundTerm("true", arrayOf())
            }

            "invocation with atom literal" {
                val result = parseQuery("'true'.")
                result.isSuccess shouldBe true
                result.certainty shouldBe MATCHED
                result.reportings should beEmpty()
                result.item shouldNotBe null
                result.item!! should beInstanceOf<PredicateInvocationQuery>()
                (result.item!! as PredicateInvocationQuery).goal shouldBe CompoundTerm("true", arrayOf())
            }
        }
    }
}) {
    override fun isolationMode() = IsolationMode.InstancePerTest
}