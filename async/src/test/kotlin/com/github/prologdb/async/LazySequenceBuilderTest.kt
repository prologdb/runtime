package com.github.prologdb.async

import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.matchers.shouldThrow
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

    "failing nested" {
        val ex = RuntimeException("Some fancy exception 1")

        val seq = buildLazySequence<Unit>(RANDOM_PRINCIPAL) {
            try {
                buildLazySequence<Unit>(this.principal) {
                    try {
                        buildLazySequence<Unit>(this.principal) {
                            throw ex
                        }.consumeAll()
                    } catch (ex: RuntimeException) {
                        throw RuntimeException(ex)
                    }
                }.consumeAll()
            } catch (ex: RuntimeException) {
                throw RuntimeException(ex)
            }
        }

        val thrown = shouldThrow<RuntimeException> {
            seq.consumeAll()
        }

        thrown.cause!!.cause shouldBe ex
    }

    "failing in nested yield all" {
        val ex = RuntimeException("Some fancy exception")

        val seq = buildLazySequence<Unit>(RANDOM_PRINCIPAL) {
            try {
                yieldAll(buildLazySequence(principal) {
                    throw ex
                })
            } catch (ex: RuntimeException) {
                throw RuntimeException("Rethrow", ex)
            }
        }

        val thrown = shouldThrow<RuntimeException> {
            seq.consumeAll()
        }

        thrown.message shouldBe "Rethrow"
        thrown.cause shouldBe ex
        seq.state shouldBe LazySequence.State.FAILED
    }

    "recovering in yield all" {
        val ex = RuntimeException("Some fancy exception")

        val seq = buildLazySequence<Unit>(RANDOM_PRINCIPAL) {
            try {
                yieldAll(buildLazySequence(principal) {
                    throw ex
                })
            } catch (ex: RuntimeException) {
                yield(Unit)
            }
        }

        seq.tryAdvance() shouldBe Unit
        seq.tryAdvance() shouldBe null
        seq.state shouldBe LazySequence.State.DEPLETED
    }

    "failing in nested await" {
        val ex = RuntimeException("Some fancy exception")

        val seq = buildLazySequence<Unit>(RANDOM_PRINCIPAL) {
            try {
                await(launchWorkableFuture(principal) {
                    throw ex
                })
            } catch (ex: RuntimeException) {
                throw RuntimeException("Rethrow", ex)
            }
        }

        val thrown = shouldThrow<RuntimeException> {
            seq.consumeAll()
        }

        thrown.message shouldBe "Rethrow"
        thrown.cause shouldBe ex
    }

    "recovering in await" {
        val ex = RuntimeException("Some fancy exception")

        val seq = buildLazySequence<Unit>(RANDOM_PRINCIPAL) {
            try {
                await(launchWorkableFuture(principal) {
                    throw ex
                })
            } catch (ex: RuntimeException) {
                yield(Unit)
            }
        }

        seq.tryAdvance() shouldBe Unit
        seq.tryAdvance() shouldBe null
        seq.state shouldBe LazySequence.State.DEPLETED
    }

    "LazySequence of" {
        val seq = LazySequence.of("foobar")

        seq.tryAdvance() shouldEqual "foobar"
        seq.tryAdvance() shouldEqual null
    }
}}
