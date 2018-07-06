package com.github.prologdb.runtime.lazysequence

import io.kotlintest.matchers.shouldEqual
import io.kotlintest.specs.FreeSpec

class LazySequenceBuilderTest : FreeSpec() { init {
    "yield - single element" {
        val seq = buildLazySequence {
            yield("foobar")
        }

        seq.tryAdvance() shouldEqual "foobar"
        seq.tryAdvance() shouldEqual null
    }

    "yield - multiple elements" {
        val seq = buildLazySequence {
            yield("foo")
            yield("bar")
        }

        seq.tryAdvance() shouldEqual "foo"
        seq.tryAdvance() shouldEqual "bar"
        seq.tryAdvance() shouldEqual null
    }

    "yieldAll - single element" {
        val seq = buildLazySequence {
            yieldAll(buildLazySequence {
                yield("foobar")
            })
        }

        seq.tryAdvance() shouldEqual "foobar"
        seq.tryAdvance() shouldEqual null
    }

    "yieldAll - multiple elements" {
        val seq = buildLazySequence {
            yieldAll(buildLazySequence {
                yield("foo")
                yield("bar")
            })
        }

        seq.tryAdvance() shouldEqual "foo"
        seq.tryAdvance() shouldEqual "bar"
        seq.tryAdvance() shouldEqual null
    }

    "yield then yieldAll" {
        val seq = buildLazySequence {
            yield("baz")

            yieldAll(buildLazySequence {
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
        val seq = buildLazySequence {
            yieldAll(buildLazySequence {
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
        val seq = buildLazySequence {
            yield("beep")
            yieldAll(buildLazySequence {
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

    "LazySequence of" {
        val seq = LazySequence.of("foobar")

        seq.tryAdvance() shouldEqual "foobar"
        seq.tryAdvance() shouldEqual null
    }
}}