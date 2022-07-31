package com.github.prologdb.parser.sequence

import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class IteratorBasedTransactionalSequenceTest : FreeSpec() {init {
    val source = listOf(1, 2, 3, 4, 5, 6, 7)

    "passthrough without markers" {
        val seq = IteratorBasedTransactionalSequence(source.iterator())

        for (index in 0..source.lastIndex) {
            seq.next() shouldBe source[index]
        }
    }

    "single level" {
        val seq = IteratorBasedTransactionalSequence(source.iterator())

        seq.mark()
        seq.next() shouldBe source[0]
        seq.next() shouldBe source[1]
        seq.next() shouldBe source[2]
        seq.rollback()

        seq.next() shouldBe source[0]
    }

    "multilevel" {
        val seq = IteratorBasedTransactionalSequence(source.iterator())

        seq.mark()
        seq.next() shouldBe source[0]
        seq.next() shouldBe source[1]
        seq.mark()
        seq.next() shouldBe source[2]
        seq.next() shouldBe source[3]
        seq.mark()
        seq.next() shouldBe source[4]
        seq.commit()

        seq.next() shouldBe source[5]

        seq.rollback()
        seq.next() shouldBe source[2]

        seq.rollback()

        for (index in 0..source.lastIndex) {
            seq.next() shouldBe source[index]
        }
    }

    "buffer clear" {
        val seq = IteratorBasedTransactionalSequence(source.iterator())

        // fill the buffer
        seq.mark()
        seq.next() shouldBe source[0]
        seq.next() shouldBe source[1]

        seq.rollback()

        seq.next() shouldBe source[0] // from buffer
        seq.next() shouldBe source[1] // from buffer
        seq.next() shouldBe source[2] // from source lazysequence, buffer should be empty now
        seq.next() shouldBe source[3] // from source lazysequence, buffer should be empty now
        seq.next() shouldBe source[4] // from source lazysequence, buffer should be empty now
    }
}}