package com.github.prologdb.parser.lexer

import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class SourceLocationAwareCharIteratorTest : FreeSpec() {init{
    "test" {
        val source = "a\nabc\nd\nef\n"

        val iterator = SourceLocationAwareCharIterator(SourceLocation(SourceUnit("testcode"), 1, 1, 0), source.asIterable().iterator())

        var next: Pair<Char, SourceLocation>

        next = iterator.next()
        next.first shouldBe 'a'
        next.second.line shouldBe 1
        next.second.column shouldBe 1

        next = iterator.next()
        next.first shouldBe '\n'
        next.second.line shouldBe 1
        next.second.column shouldBe 2

        next = iterator.next()
        next.first shouldBe 'a'
        next.second.line shouldBe 2
        next.second.column shouldBe 1

        next = iterator.next()
        next.first shouldBe 'b'
        next.second.line shouldBe 2
        next.second.column shouldBe 2

        next = iterator.next()
        next.first shouldBe 'c'
        next.second.line shouldBe 2
        next.second.column shouldBe 3

        next = iterator.next()
        next.first shouldBe '\n'
        next.second.line shouldBe 2
        next.second.column shouldBe 4

        next = iterator.next()
        next.first shouldBe 'd'
        next.second.line shouldBe 3
        next.second.column shouldBe 1

        next = iterator.next()
        next.first shouldBe '\n'
        next.second.line shouldBe 3
        next.second.column shouldBe 2

        next = iterator.next()
        next.first shouldBe 'e'
        next.second.line shouldBe 4
        next.second.column shouldBe 1

        next = iterator.next()
        next.first shouldBe 'f'
        next.second.line shouldBe 4
        next.second.column shouldBe 2

        next = iterator.next()
        next.first shouldBe '\n'
        next.second.line shouldBe 4
        next.second.column shouldBe 3
    }
}}