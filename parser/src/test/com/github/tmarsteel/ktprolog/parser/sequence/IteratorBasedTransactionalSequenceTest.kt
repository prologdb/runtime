package com.github.tmarsteel.ktprolog.parser.sequence

import io.kotlintest.specs.FreeSpec

class IteratorBasedTransactionalSequenceTest : FreeSpec() {init {
    val source = listOf(1, 2, 3, 4, 5, 6, 7)

    "passthrough without markers" {
        val seq = IteratorBasedTransactionalSequence(source.iterator())

        for (index in 0..source.lastIndex) {
            seq.next() shouldEqual source[index]
        }
    }

    "single level" {
        val seq = IteratorBasedTransactionalSequence(source.iterator())

        seq.mark()
        seq.next() shouldEqual source[0]
        seq.next() shouldEqual source[1]
        seq.next() shouldEqual source[2]
        seq.rollback()

        seq.next() shouldEqual source[0]
    }

    "multilevel" {
        val seq = IteratorBasedTransactionalSequence(source.iterator())

        seq.mark()
        seq.next() shouldEqual source[0]
        seq.next() shouldEqual source[1]
        seq.mark()
        seq.next() shouldEqual source[2]
        seq.next() shouldEqual source[3]
        seq.mark()
        seq.next() shouldEqual source[4]
        seq.commit()

        seq.next() shouldEqual source[5]

        seq.rollback()
        seq.next() shouldEqual source[2]

        seq.rollback()

        for (index in 0..source.lastIndex) {
            seq.next() shouldEqual source[index]
        }
    }
}}