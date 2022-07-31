package com.github.prologdb.parser.lexer

import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class LineEndingNormalizerTest : FreeSpec(){init {
    val source = "a\nabc\nab\rabcd\r\nhht\r"

    "test" {
        val normalizer = LineEndingNormalizer(source.asIterable().iterator())

        normalizer.next() shouldBe 'a'
        normalizer.next() shouldBe '\n'
        normalizer.next() shouldBe 'a'
        normalizer.next() shouldBe 'b'
        normalizer.next() shouldBe 'c'
        normalizer.next() shouldBe '\n'
        normalizer.next() shouldBe 'a'
        normalizer.next() shouldBe 'b'
        normalizer.next() shouldBe '\n'
        normalizer.next() shouldBe 'a'
        normalizer.next() shouldBe 'b'
        normalizer.next() shouldBe 'c'
        normalizer.next() shouldBe 'd'
        normalizer.next() shouldBe '\n'
        normalizer.next() shouldBe 'h'
        normalizer.next() shouldBe 'h'
        normalizer.next() shouldBe 't'
        normalizer.next() shouldBe '\n'

        normalizer.hasNext() shouldBe false
        shouldThrow<NoSuchElementException> {
            normalizer.next()
        }
    }

    "empty" {
        val normalizer = LineEndingNormalizer("".asIterable().iterator())

        normalizer.hasNext() shouldBe false
        shouldThrow<NoSuchElementException> {
            normalizer.next()
        }
    }
}}