package com.github.prologdb.async

import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class FlatMapLazySequenceTest : FreeSpec({
    "1:1" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("foo")
            null
        }.flatMapRemaining<String, String> {
            yield(it + "bar")
            null
        }

        sequence.tryAdvance() shouldBe "foobar"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }

    "1:n" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("foo")
            null
        }.flatMapRemaining<String, String> {
            yield(it + "bar")
            yield(it + "baz")
            null
        }

        sequence.tryAdvance() shouldBe "foobar"
        sequence.tryAdvance() shouldBe "foobaz"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }

    "n:1" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("bar")
            yield("baz")
            null
        }.flatMapRemaining<String, String> {
            yield("foo$it")
            null
        }

        sequence.tryAdvance() shouldBe "foobar"
        sequence.tryAdvance() shouldBe "foobaz"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }

    "m:n" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("bar")
            yield("fuzz")
            null
        }.flatMapRemaining<String, String> {
            yield(it + "foo")
            yield(it + "fizz")
            null
        }

        sequence.tryAdvance() shouldBe "barfoo"
        sequence.tryAdvance() shouldBe "barfizz"
        sequence.tryAdvance() shouldBe "fuzzfoo"
        sequence.tryAdvance() shouldBe "fuzzfizz"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }
})
