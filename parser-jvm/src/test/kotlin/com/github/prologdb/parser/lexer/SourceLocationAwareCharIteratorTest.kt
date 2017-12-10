package com.github.prologdb.parser.lexer

import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.specs.FreeSpec

class SourceLocationAwareCharIteratorTest : FreeSpec() {init{
    "test" {
        val source = "a\nabc\nd\nef\n"

        val iterator = SourceLocationAwareCharIterator(SourceLocation(SourceUnit("testcode"), 1, 1, 0), source.asIterable().iterator())

        var next: Pair<Char, SourceLocation>

        next = iterator.next()
        next.first shouldEqual 'a'
        next.second.line shouldEqual 1
        next.second.column shouldEqual 1

        next = iterator.next()
        next.first shouldEqual '\n'
        next.second.line shouldEqual 1
        next.second.column shouldEqual 2

        next = iterator.next()
        next.first shouldEqual 'a'
        next.second.line shouldEqual 2
        next.second.column shouldEqual 1

        next = iterator.next()
        next.first shouldEqual 'b'
        next.second.line shouldEqual 2
        next.second.column shouldEqual 2

        next = iterator.next()
        next.first shouldEqual 'c'
        next.second.line shouldEqual 2
        next.second.column shouldEqual 3

        next = iterator.next()
        next.first shouldEqual '\n'
        next.second.line shouldEqual 2
        next.second.column shouldEqual 4

        next = iterator.next()
        next.first shouldEqual 'd'
        next.second.line shouldEqual 3
        next.second.column shouldEqual 1

        next = iterator.next()
        next.first shouldEqual '\n'
        next.second.line shouldEqual 3
        next.second.column shouldEqual 2

        next = iterator.next()
        next.first shouldEqual 'e'
        next.second.line shouldEqual 4
        next.second.column shouldEqual 1

        next = iterator.next()
        next.first shouldEqual 'f'
        next.second.line shouldEqual 4
        next.second.column shouldEqual 2

        next = iterator.next()
        next.first shouldEqual '\n'
        next.second.line shouldEqual 4
        next.second.column shouldEqual 3
    }
}}