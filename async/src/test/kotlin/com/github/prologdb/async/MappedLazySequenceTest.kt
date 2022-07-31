package com.github.prologdb.async

import com.github.prologdb.async.LazySequence.State.*
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.BehaviorSpec
import io.kotest.matchers.shouldBe

class MappedLazySequenceTest : BehaviorSpec() { init {
    Given("a fixed source") {
        val source = LazySequence.of(1, 2, 3)
        val mapped = MappedLazySequence(source) { it + 1 }

        When("used") {
            Then("It should behave according to contract") {
                mapped.state shouldBe PENDING
                mapped.step() shouldBe RESULTS_AVAILABLE
                mapped.tryAdvance() shouldBe 2
                mapped.state shouldBe RESULTS_AVAILABLE
                mapped.tryAdvance() shouldBe 3
                mapped.state shouldBe RESULTS_AVAILABLE
                mapped.tryAdvance() shouldBe 4
                mapped.state shouldBe DEPLETED
                mapped.tryAdvance() shouldBe null
            }
        }
    }

    Given("a yielding source") {
        val source = buildLazySequence<Int>(IrrelevantPrincipal) {
            yield(1)
            yield(2)
            return@buildLazySequence 3
        }

        val mapped = MappedLazySequence(source) { it }
        When("used") {
            Then("It should behave according to contract") {
                mapped.state shouldBe PENDING
                mapped.step() shouldBe RESULTS_AVAILABLE
                mapped.tryAdvance() shouldBe 1
                mapped.state shouldBe PENDING
                mapped.step() shouldBe RESULTS_AVAILABLE
                mapped.tryAdvance() shouldBe 2
                mapped.state shouldBe PENDING
                mapped.step() shouldBe RESULTS_AVAILABLE
                mapped.tryAdvance() shouldBe 3
                mapped.state shouldBe DEPLETED
                mapped.tryAdvance() shouldBe null
            }
        }
    }

    Given("a yielding and then throwing source") {
        val source = buildLazySequence<Int>(IrrelevantPrincipal) {
            yield(1)
            throw RuntimeException("error")
        }
        val mapped = MappedLazySequence(source) { it }
        When("used") {
            Then("it should yield the first result and then properly fall into the FAIL state") {
                mapped.state shouldBe PENDING
                mapped.step() shouldBe RESULTS_AVAILABLE
                mapped.tryAdvance() shouldBe 1
                mapped.state shouldBe PENDING
                mapped.step() shouldBe FAILED
                val ex = shouldThrow<RuntimeException> {
                    mapped.tryAdvance()
                }
                ex.message shouldBe "error"
            }
        }
    }

    Given("a source that throws an exception") {
        val source = buildLazySequence<Unit>(IrrelevantPrincipal) {
            throw RuntimeException("error")
        }

        val mapped = MappedLazySequence(source) { "mapped" }

        When("tryAdvance") {
            Then("it should throw an error") {
                val ex = shouldThrow<RuntimeException> {
                    mapped.tryAdvance()
                }
                ex.message shouldBe "error"
            }
            Then("the state should be FAILED") {
                try {
                    mapped.tryAdvance()
                } catch (ignored: Exception) {
                }

                mapped.state shouldBe FAILED
            }
            Then("tryAdvance should throw the same exception again") {
                try {
                    mapped.tryAdvance()
                } catch (ignored: Exception) {
                }

                val ex = shouldThrow<RuntimeException> {
                    mapped.tryAdvance()
                }
                ex.message shouldBe "error"
            }
        }
    }
    Given("A working source and a throwing mapper") {
        val source = LazySequence.of(Unit)
        val mapped = MappedLazySequence(source) { throw RuntimeException("error") }

        When("tryAdvance") {
            Then("it should throw an error") {
                val ex = shouldThrow<RuntimeException> {
                    mapped.tryAdvance()
                }
                ex.message shouldBe "error"
            }
            Then("the state should be FAILED") {
                try {
                    mapped.tryAdvance()
                } catch (ignored: Exception) {
                }

                mapped.state shouldBe FAILED
            }
            Then("tryAdvance should throw the same exception again") {
                try {
                    mapped.tryAdvance()
                } catch (ignored: Exception) {
                }

                val ex = shouldThrow<RuntimeException> {
                    mapped.tryAdvance()
                }
                ex.message shouldBe "error"
            }
        }
    }
}
}
