package com.github.prologdb.async

import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.FreeSpec

class FlatMapLazySequenceTest : FreeSpec({
    "1:1" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("foo")
        }.flatMapRemaining<String, String> {
            yield(it + "bar")
        }

        sequence.tryAdvance() shouldBe "foobar"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }

    "1:n" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("foo")
        }.flatMapRemaining<String, String> {
            yield(it + "bar")
            yield(it + "baz")
        }

        sequence.tryAdvance() shouldBe "foobar"
        sequence.tryAdvance() shouldBe "foobaz"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }

    "n:1" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("bar")
            yield("baz")
        }.flatMapRemaining<String, String> {
            yield("foo$it")
        }

        sequence.tryAdvance() shouldBe "foobar"
        sequence.tryAdvance() shouldBe "foobaz"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }

    "m:n" {
        val sequence = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("bar")
            yield("fuzz")
        }.flatMapRemaining<String, String> {
            yield(it + "foo")
            yield(it + "fizz")
        }

        sequence.tryAdvance() shouldBe "barfoo"
        sequence.tryAdvance() shouldBe "barfizz"
        sequence.tryAdvance() shouldBe "fuzzfoo"
        sequence.tryAdvance() shouldBe "fuzzfizz"
        sequence.state shouldBe LazySequence.State.DEPLETED
    }
})
