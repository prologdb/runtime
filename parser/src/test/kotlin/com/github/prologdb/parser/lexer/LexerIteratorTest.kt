package com.github.prologdb.parser.lexer

import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.term.PrologNumber
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class LexerIteratorTest : FreeSpec() {init{
    "test" - {
        val source = """predicate(arg).
            predicate(arg1, arg2).
            ruleHeadPredicate(arg1, X) :- goal1(arg1), goal2(X).
            foo(1,2,3.412)  .
            a+1.
            "this is a simple string"  "this \a is \b \v a \e \t string \n with \r \" escaped \stuff"

            % this signle-line comment should be ignored
            foo bar % this comment should be ignored, too
            /* ignore! */
            bar /* ignore! */ foo
            'atom literal'
            foo =@= bar
            bar \=@= foo
            922337203685477581000000000
        """
        val lexer = LexerIterator(source.asIterable().iterator(), SourceLocation(SourceUnit("testcode"), 1, 1, 0))

        var next: Token

        "line 1" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "predicate"
            next.location.start.line shouldBe 1
            next.location.start.column shouldBe 1
            next.location.end.line shouldBe 1
            next.location.end.column shouldBe 9

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_OPEN
            next.location.start.line shouldBe 1
            next.location.start.column shouldBe 10
            next.location.end.line shouldBe 1
            next.location.end.column shouldBe 10

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "arg"
            next.location.start.line shouldBe 1
            next.location.start.column shouldBe 11
            next.location.end.line shouldBe 1
            next.location.end.column shouldBe 13

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_CLOSE
            next.location.start.line shouldBe 1
            next.location.start.column shouldBe 14
            next.location.end.line shouldBe 1
            next.location.end.column shouldBe 14

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.FULL_STOP
            next.location.start.line shouldBe 1
            next.location.start.column shouldBe 15
            next.location.end.line shouldBe 1
            next.location.end.column shouldBe 15
        }

        "line 2" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "predicate"
            next.location.start.line shouldBe 2
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 2
            next.location.end.column shouldBe 21

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_OPEN
            next.location.start.line shouldBe 2
            next.location.start.column shouldBe 22
            next.location.end.line shouldBe 2
            next.location.end.column shouldBe 22

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "arg1"
            next.location.start.line shouldBe 2
            next.location.start.column shouldBe 23
            next.location.end.line shouldBe 2
            next.location.end.column shouldBe 26

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.COMMA
            next.location.start.line shouldBe 2
            next.location.start.column shouldBe 27
            next.location.end.line shouldBe 2
            next.location.end.column shouldBe 27

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "arg2"
            next.location.start.line shouldBe 2
            next.location.start.column shouldBe 29
            next.location.end.line shouldBe 2
            next.location.end.column shouldBe 32

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_CLOSE
            next.location.start.line shouldBe 2
            next.location.start.column shouldBe 33
            next.location.end.line shouldBe 2
            next.location.end.column shouldBe 33

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.FULL_STOP
            next.location.start.line shouldBe 2
            next.location.start.column shouldBe 34
            next.location.end.line shouldBe 2
            next.location.end.column shouldBe 34
        }

        "line 3" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "ruleHeadPredicate"
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 29

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_OPEN
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 30
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 30

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "arg1"
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 31
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 34

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.COMMA
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 35
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 35

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "X"
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 37
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 37

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_CLOSE
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 38
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 38

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.HEAD_QUERY_SEPARATOR
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 40
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 41

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "goal1"
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 43
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 47

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_OPEN
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 48
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 48

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "arg1"
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 49
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 52

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_CLOSE
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 53
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 53

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.COMMA
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 54
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 54

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "goal2"
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 56
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 60

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_OPEN
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 61
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 61

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "X"
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 62
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 62

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_CLOSE
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 63
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 63

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.FULL_STOP
            next.location.start.line shouldBe 3
            next.location.start.column shouldBe 64
            next.location.end.line shouldBe 3
            next.location.end.column shouldBe 64
        }

        "line 4" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "foo"
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 15

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_OPEN
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 16
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 16

            next = lexer.next()
            assert(next is NumericLiteralToken)
            (next as NumericLiteralToken).number shouldBe PrologNumber(1)
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 17
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 17

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.COMMA
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 18
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 18

            next = lexer.next()
            assert(next is NumericLiteralToken)
            (next as NumericLiteralToken).number shouldBe PrologNumber(2)
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 19
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 19

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.COMMA
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 20
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 20

            next = lexer.next()
            assert(next is NumericLiteralToken)
            (next as NumericLiteralToken).number shouldBe PrologNumber("3.412")
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 21
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 25

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PARENT_CLOSE
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 26
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 26

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.FULL_STOP
            next.location.start.line shouldBe 4
            next.location.start.column shouldBe 29
            next.location.end.line shouldBe 4
            next.location.end.column shouldBe 29
        }

        "line 5" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "a"
            next.location.start.line shouldBe 5
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 5
            next.location.end.column shouldBe 13

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.PLUS
            next.location.start.line shouldBe 5
            next.location.start.column shouldBe 14
            next.location.end.line shouldBe 5
            next.location.end.column shouldBe 14

            next = lexer.next()
            assert(next is NumericLiteralToken)
            (next as NumericLiteralToken).number shouldBe PrologNumber(1L)
            next.location.start.line shouldBe 5
            next.location.start.column shouldBe 15
            next.location.end.line shouldBe 5
            next.location.end.column shouldBe 15

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.FULL_STOP
            next.location.start.line shouldBe 5
            next.location.start.column shouldBe 16
            next.location.end.line shouldBe 5
            next.location.end.column shouldBe 16
        }

        "line 6" {
            next = lexer.next()
            assert(next is StringLiteralToken)
            (next as StringLiteralToken).content shouldBe "this is a simple string"
            next.location.start.line shouldBe 6
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 6
            next.location.end.column shouldBe 37

            next = lexer.next()
            assert(next is StringLiteralToken)
            (next as StringLiteralToken).content shouldBe "this \u0007 is \b \u000B a \u001B \t string \n with \r \" escaped stuff"
            next.location.start.line shouldBe 6
            next.location.start.column shouldBe 40
            next.location.end.line shouldBe 6
            next.location.end.column shouldBe 101
        }

        "line 9" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "foo"
            next.location.start.line shouldBe 9
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 9
            next.location.end.column shouldBe 15

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "bar"
            next.location.start.line shouldBe 9
            next.location.start.column shouldBe 17
            next.location.end.line shouldBe 9
            next.location.end.column shouldBe 19
        }

        "line 11" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "bar"
            next.location.start.line shouldBe 11
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 11
            next.location.end.column shouldBe 15

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "foo"
            next.location.start.line shouldBe 11
            next.location.start.column shouldBe 31
            next.location.end.line shouldBe 11
            next.location.end.column shouldBe 33
        }

        "line 12" {
            next = lexer.next()
            next.type shouldBe TokenType.ATOM_LITERAL
            next.location.start.line shouldBe 12
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 12
            next.location.end.column shouldBe 26
        }

        "line 13" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "foo"
            next.location.start.line shouldBe 13
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 13
            next.location.end.column shouldBe 15

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.TERM_VARIANT
            next.location.start.line shouldBe 13
            next.location.start.column shouldBe 17
            next.location.end.line shouldBe 13
            next.location.end.column shouldBe 19

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "bar"
            next.location.start.line shouldBe 13
            next.location.start.column shouldBe 21
            next.location.end.line shouldBe 13
            next.location.end.column shouldBe 23
        }

        "line 14" {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "bar"
            next.location.start.line shouldBe 14
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 14
            next.location.end.column shouldBe 15

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldBe Operator.TERM_NOT_VARIANT
            next.location.start.line shouldBe 14
            next.location.start.column shouldBe 17
            next.location.end.line shouldBe 14
            next.location.end.column shouldBe 20

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldBe "foo"
            next.location.start.line shouldBe 14
            next.location.start.column shouldBe 22
            next.location.end.line shouldBe 14
            next.location.end.column shouldBe 24
        }

        "line 15" {
            next = lexer.next()
            assert(next is NumericLiteralToken)
            (next as NumericLiteralToken).number shouldBe PrologNumber("922337203685477581000000000")
            next.location.start.line shouldBe 15
            next.location.start.column shouldBe 13
            next.location.end.line shouldBe 15
            next.location.end.column shouldBe 39
        }

        "eof" {
            lexer.hasNext() shouldBe false
        }
    }
}}