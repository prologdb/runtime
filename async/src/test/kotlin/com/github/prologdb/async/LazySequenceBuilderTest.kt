package com.github.prologdb.async

import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.specs.FreeSpec
import java.util.concurrent.CompletableFuture

class LazySequenceBuilderTest : FreeSpec() { init {
    "yield - single element" {
        val seq = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("foobar")
        }

        seq.tryAdvance() shouldEqual "foobar"
        seq.tryAdvance() shouldEqual null
    }

    "yield - multiple elements" {
        val seq = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("foo")
            yield("bar")
        }

        seq.tryAdvance() shouldEqual "foo"
        seq.tryAdvance() shouldEqual "bar"
        seq.tryAdvance() shouldEqual null
    }

    "yieldAll - single element" {
        val seq = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yieldAll(buildLazySequence(principal) {
                yield("foobar")
            })
        }

        seq.tryAdvance() shouldEqual "foobar"
        seq.tryAdvance() shouldEqual null
    }

    "yieldAll - multiple elements" {
        val seq = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yieldAll(buildLazySequence(principal) {
                yield("foo")
                yield("bar")
            })
        }

        seq.tryAdvance() shouldEqual "foo"
        seq.tryAdvance() shouldEqual "bar"
        seq.tryAdvance() shouldEqual null
    }

    "yield then yieldAll" {
        val seq = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("baz")

            yieldAll(buildLazySequence(principal) {
                yield("foo")
                yield("bar")
            })
        }

        seq.tryAdvance() shouldEqual "baz"
        seq.tryAdvance() shouldEqual "foo"
        seq.tryAdvance() shouldEqual "bar"
        seq.tryAdvance() shouldEqual null
    }

    "yieldAll then yield" {
        val seq = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yieldAll(buildLazySequence(principal) {
                yield("foo")
                yield("bar")
            })
            yield("baz")
        }

        seq.tryAdvance() shouldEqual "foo"
        seq.tryAdvance() shouldEqual "bar"
        seq.tryAdvance() shouldEqual "baz"
        seq.tryAdvance() shouldEqual null
    }

    "yield then yieldAll then yield" {
        val seq = buildLazySequence<String>(RANDOM_PRINCIPAL) {
            yield("beep")
            yieldAll(buildLazySequence(principal) {
                yield("foo")
                yield("bar")
            })
            yield("baz")
        }

        seq.tryAdvance() shouldEqual "beep"
        seq.tryAdvance() shouldEqual "foo"
        seq.tryAdvance() shouldEqual "bar"
        seq.tryAdvance() shouldEqual "baz"
        seq.tryAdvance() shouldEqual null
    }

    "failing await" {
        val exceptionToThrow = RuntimeException("Some fancy exception")
        val failingFuture = CompletableFuture<Unit>()

        var exceptionCaughtInLS: Throwable? = null
        val seq = buildLazySequence<Unit>(RANDOM_PRINCIPAL) {
            try {
                await(failingFuture)
            }
            catch (ex: Throwable) {
                exceptionCaughtInLS = ex
            }
        }

        seq.step() // now hangs on the future

        failingFuture.completeExceptionally(exceptionToThrow)

        seq.step() shouldBe LazySequence.State.DEPLETED // this should give the error to the catch code in the sequence
        exceptionCaughtInLS shouldBe exceptionToThrow
        seq.tryAdvance() shouldBe null
    }

    "LazySequence of" {
        val seq = LazySequence.of("foobar")

        seq.tryAdvance() shouldEqual "foobar"
        seq.tryAdvance() shouldEqual null
    }
}}
