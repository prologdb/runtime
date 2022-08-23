package com.github.prologdb.async

import com.github.prologdb.async.LazySequence.State.*
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.should
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.beTheSameInstanceAs

class SkippingLazySequenceTest : FreeSpec({
    "fixed source" {
        val source = LazySequence.of(1, 2, 3, 4, 5)
        val skipped = SkippingLazySequence(source, 2)

        skipped.state shouldBe PENDING
        skipped.step() shouldBe RESULTS_AVAILABLE
        skipped.tryAdvance() shouldBe 3
        skipped.step() shouldBe RESULTS_AVAILABLE
        skipped.tryAdvance() shouldBe 4
        skipped.step() shouldBe RESULTS_AVAILABLE
        skipped.tryAdvance() shouldBe 5
        skipped.state shouldBe DEPLETED
        skipped.step() shouldBe DEPLETED
    }

    "yielding source" {
        val source = buildLazySequence<Int>(IrrelevantPrincipal) {
            yield(1)
            yield(2)
            yield(3)
            yield(4)
            5
        }
        val skipped = SkippingLazySequence(source, 2)

        skipped.state shouldBe PENDING
        skipped.step() shouldBe PENDING
        skipped.step() shouldBe PENDING
        skipped.step() shouldBe RESULTS_AVAILABLE
        skipped.tryAdvance() shouldBe 3
        skipped.step() shouldBe RESULTS_AVAILABLE
        skipped.tryAdvance() shouldBe 4
        skipped.step() shouldBe RESULTS_AVAILABLE
        skipped.tryAdvance() shouldBe 5
        skipped.state shouldBe DEPLETED
        skipped.step() shouldBe DEPLETED
    }

    "depletion during skip phase" {
        val source = buildLazySequence<Int>(IrrelevantPrincipal) {
            yield(1)
            2
        }

        val skipped = SkippingLazySequence(source, 3)
        skipped.state shouldBe PENDING
        skipped.step() shouldBe PENDING
        skipped.step() shouldBe DEPLETED
        skipped.state shouldBe DEPLETED
        skipped.tryAdvance() shouldBe null
    }

    "failure during skip phase" {
        val ex = RuntimeException("some error")
        val source = buildLazySequence<Int>(IrrelevantPrincipal) {
            yield(1)
            yield(2)
            throw ex
        }

        val skipped = SkippingLazySequence(source, 3)
        skipped.state shouldBe PENDING
        skipped.step() shouldBe PENDING
        skipped.step() shouldBe PENDING
        skipped.step() shouldBe FAILED
        skipped.state shouldBe FAILED
        val actualEx = shouldThrow<RuntimeException> {
            skipped.tryAdvance()
        }

        actualEx should beTheSameInstanceAs(ex)
    }
})