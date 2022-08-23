package com.github.prologdb.async

import com.github.prologdb.async.LazySequence.State.*
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.should
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.beTheSameInstanceAs

class DeterministicLazySequenceTest : FreeSpec({
    val unfinishedEx = RuntimeException("unfinished")
    val multipleEx = RuntimeException("multiple")

    "deterministic" - {
        val emptyEx = RuntimeException("empty")

        "fixed source with one item" {
            val source = LazySequence.of("foo")
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, { throw emptyEx })

            deterministic.state shouldBe RESULTS_AVAILABLE
            deterministic.step() shouldBe RESULTS_AVAILABLE
            deterministic.tryAdvance() shouldBe "foo"
            deterministic.state shouldBe DEPLETED
            deterministic.step() shouldBe DEPLETED
        }

        "fixed source with two items" {
            val source = LazySequence.of("foo", "bar")
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, { throw emptyEx })

            deterministic.state shouldBe RESULTS_AVAILABLE
            deterministic.step() shouldBe RESULTS_AVAILABLE
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            ex should beTheSameInstanceAs(multipleEx)
            deterministic.state shouldBe FAILED
            deterministic.step() shouldBe FAILED
        }

        "fixed source with no items" {
            val source = LazySequence.empty<String>()
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, { throw emptyEx })

            deterministic.state shouldBe FAILED
            deterministic.step() shouldBe FAILED
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            ex should beTheSameInstanceAs(emptyEx)
        }

        "yielding source with one item" {
            val source = buildLazySequence(IrrelevantPrincipal) {
                "foo"
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, { throw emptyEx })

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe RESULTS_AVAILABLE
            deterministic.tryAdvance() shouldBe "foo"
            deterministic.state shouldBe DEPLETED
            deterministic.step() shouldBe DEPLETED
        }

        "yielding source with one item but dangling suspension point" {
            val source = buildLazySequence(IrrelevantPrincipal) {
                yield("foo")
                null
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, { throw emptyEx })

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe RESULTS_AVAILABLE
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            ex should beTheSameInstanceAs(unfinishedEx)
            deterministic.state shouldBe FAILED
            deterministic.step() shouldBe FAILED
            deterministic.state shouldBe FAILED
        }

        "yielding source with two items" {
            val source = buildLazySequence<String>(IrrelevantPrincipal) {
                yield("foo")
                "bar"
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, { throw emptyEx })

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe RESULTS_AVAILABLE
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            assert(ex == unfinishedEx || ex == multipleEx)
            deterministic.state shouldBe FAILED
            deterministic.step() shouldBe FAILED
        }

        "yielding source with no items" {
            val source = buildLazySequence<String>(IrrelevantPrincipal) {
                null
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, { throw emptyEx })

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe FAILED
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            ex should beTheSameInstanceAs(emptyEx)
        }
    }

    "semi-deterministic" - {
        "fixed source with one item" {
            val source = LazySequence.of("foo")
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, null)

            deterministic.state shouldBe RESULTS_AVAILABLE
            deterministic.step() shouldBe RESULTS_AVAILABLE
            deterministic.tryAdvance() shouldBe "foo"
            deterministic.state shouldBe DEPLETED
            deterministic.step() shouldBe DEPLETED
        }

        "fixed source with two items" {
            val source = LazySequence.of("foo", "bar")
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, null)

            deterministic.state shouldBe RESULTS_AVAILABLE
            deterministic.step() shouldBe RESULTS_AVAILABLE
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            ex should beTheSameInstanceAs(multipleEx)
            deterministic.state shouldBe FAILED
            deterministic.step() shouldBe FAILED
        }

        "fixed source with no items" {
            val source = LazySequence.empty<String>()
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, null)

            deterministic.state shouldBe DEPLETED
            deterministic.step() shouldBe DEPLETED
            deterministic.tryAdvance() shouldBe null
        }

        "yielding source with one item" {
            val source = buildLazySequence(IrrelevantPrincipal) {
                "foo"
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, null)

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe RESULTS_AVAILABLE
            deterministic.tryAdvance() shouldBe "foo"
            deterministic.state shouldBe DEPLETED
            deterministic.step() shouldBe DEPLETED
        }

        "yielding source with one item but dangling suspension point" {
            val source = buildLazySequence(IrrelevantPrincipal) {
                yield("foo")
                null
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, null)

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe RESULTS_AVAILABLE
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            ex should beTheSameInstanceAs(unfinishedEx)
            deterministic.state shouldBe FAILED
            deterministic.step() shouldBe FAILED
            deterministic.state shouldBe FAILED
        }

        "yielding source with two items" {
            val source = buildLazySequence<String>(IrrelevantPrincipal) {
                yield("foo")
                "bar"
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, null)

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe RESULTS_AVAILABLE
            val ex = shouldThrow<Exception> {
                deterministic.tryAdvance()
            }
            assert(ex == unfinishedEx || ex == multipleEx)
            deterministic.state shouldBe FAILED
            deterministic.step() shouldBe FAILED
        }

        "yielding source with no items" {
            val source = buildLazySequence<String>(IrrelevantPrincipal) {
                null
            }
            val deterministic =
                DeterministicLazySequence(source, { throw unfinishedEx }, { throw multipleEx }, null)

            deterministic.state shouldBe PENDING
            deterministic.step() shouldBe DEPLETED
            deterministic.tryAdvance() shouldBe null
        }
    }
})