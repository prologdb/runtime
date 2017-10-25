package com.github.tmarsteel.ktprolog.parser.lexer

import com.github.tmarsteel.ktprolog.parser.source.SourceUnit
import io.kotlintest.specs.FreeSpec

class LexerIteratorTest : FreeSpec() {init{
    "test" {
        val source = """predicate(arg).
            predicate(arg1, arg2).
            ruleHeadPredicate(arg1, X) :- goal1(arg1), goal2(X).
        """
        val lexer = LexerIterator(SourceUnit("testcode"), source.asIterable().iterator())

        var next: Token

        "line 1" - {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "predicate"
            next.location.start.line shouldEqual 1
            next.location.start.column shouldEqual 1
            next.location.end.line shouldEqual 1
            next.location.end.column shouldEqual 9

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_OPEN
            next.location.start.line shouldEqual 1
            next.location.start.column shouldEqual 10
            next.location.end.line shouldEqual 1
            next.location.end.column shouldEqual 10

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "arg"
            next.location.start.line shouldEqual 1
            next.location.start.column shouldEqual 11
            next.location.end.line shouldEqual 1
            next.location.end.column shouldEqual 13

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_CLOSE
            next.location.start.line shouldEqual 1
            next.location.start.column shouldEqual 14
            next.location.end.line shouldEqual 1
            next.location.end.column shouldEqual 14

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.FULL_STOP
            next.location.start.line shouldEqual 1
            next.location.start.column shouldEqual 15
            next.location.end.line shouldEqual 1
            next.location.end.column shouldEqual 15
        }

        "line 2" - {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "predicate"
            next.location.start.line shouldEqual 2
            next.location.start.column shouldEqual 13
            next.location.end.line shouldEqual 2
            next.location.end.column shouldEqual 21

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_OPEN
            next.location.start.line shouldEqual 2
            next.location.start.column shouldEqual 22
            next.location.end.line shouldEqual 2
            next.location.end.column shouldEqual 22

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "arg1"
            next.location.start.line shouldEqual 2
            next.location.start.column shouldEqual 23
            next.location.end.line shouldEqual 2
            next.location.end.column shouldEqual 26

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.COMMA
            next.location.start.line shouldEqual 2
            next.location.start.column shouldEqual 27
            next.location.end.line shouldEqual 2
            next.location.end.column shouldEqual 27

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "arg2"
            next.location.start.line shouldEqual 2
            next.location.start.column shouldEqual 29
            next.location.end.line shouldEqual 2
            next.location.end.column shouldEqual 32

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_CLOSE
            next.location.start.line shouldEqual 2
            next.location.start.column shouldEqual 33
            next.location.end.line shouldEqual 2
            next.location.end.column shouldEqual 33

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.FULL_STOP
            next.location.start.line shouldEqual 2
            next.location.start.column shouldEqual 34
            next.location.end.line shouldEqual 2
            next.location.end.column shouldEqual 34
        }

        "line 3" - {
            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "ruleHeadPredicate"
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 13
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 29

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_OPEN
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 30
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 30

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "arg1"
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 31
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 34

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.COMMA
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 35
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 35

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "X"
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 37
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 37

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_CLOSE
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 38
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 38

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.HEAD_QUERY_SEPARATOR
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 40
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 41

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "goal1"
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 43
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 47

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_OPEN
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 48
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 48

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "arg1"
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 49
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 52

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_CLOSE
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 53
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 53

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.COMMA
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 54
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 54

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "goal2"
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 56
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 60

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_OPEN
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 61
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 61

            next = lexer.next()
            assert(next is IdentifierToken)
            (next as IdentifierToken).textContent shouldEqual "X"
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 62
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 62

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.PARENT_CLOSE
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 63
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 63

            next = lexer.next()
            assert(next is OperatorToken)
            (next as OperatorToken).operator shouldEqual Operator.FULL_STOP
            next.location.start.line shouldEqual 3
            next.location.start.column shouldEqual 64
            next.location.end.line shouldEqual 3
            next.location.end.column shouldEqual 64
        }
    }
}}