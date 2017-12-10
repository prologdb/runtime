package com.github.prologdb.parser.lexer

import io.kotlintest.matchers.shouldEqual
import io.kotlintest.matchers.shouldThrow
import io.kotlintest.specs.FreeSpec

class LineEndingNormalizerTest : FreeSpec(){init {
    val source = "a\nabc\nab\rabcd\r\nhht\r"

    "test" {
        val normalizer = LineEndingNormalizer(source.asIterable().iterator())

        normalizer.next() shouldEqual 'a'
        normalizer.next() shouldEqual '\n'
        normalizer.next() shouldEqual 'a'
        normalizer.next() shouldEqual 'b'
        normalizer.next() shouldEqual 'c'
        normalizer.next() shouldEqual '\n'
        normalizer.next() shouldEqual 'a'
        normalizer.next() shouldEqual 'b'
        normalizer.next() shouldEqual '\n'
        normalizer.next() shouldEqual 'a'
        normalizer.next() shouldEqual 'b'
        normalizer.next() shouldEqual 'c'
        normalizer.next() shouldEqual 'd'
        normalizer.next() shouldEqual '\n'
        normalizer.next() shouldEqual 'h'
        normalizer.next() shouldEqual 'h'
        normalizer.next() shouldEqual 't'
        normalizer.next() shouldEqual '\n'

        normalizer.hasNext() shouldEqual false
        shouldThrow<NoSuchElementException> {
            normalizer.next()
        }
    }

    "empty" {
        val normalizer = LineEndingNormalizer("".asIterable().iterator())

        normalizer.hasNext() shouldEqual false
        shouldThrow<NoSuchElementException> {
            normalizer.next()
        }
    }
}}