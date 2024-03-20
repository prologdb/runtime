package com.github.prologdb.async

import io.kotest.core.spec.IsolationMode
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class WindowsLazySequenceTest : FreeSpec({
    val source = buildLazySequence(IrrelevantPrincipal) {
        yield(1)
        yield(2)
        yield(3)
        yield(4)
        yield(5)
        yield(6)
        yield(7)
        yield(8)
        9
    }

    "size = 1" - {
        "step = 1, partials = false" {
            val windowed = WindowedLazySequence(source, 1, 1, false)
            windowed.remainingToList().get() shouldBe listOf(
                listOf(1),
                listOf(2),
                listOf(3),
                listOf(4),
                listOf(5),
                listOf(6),
                listOf(7),
                listOf(8),
                listOf(9),
            )
        }
        "step = 2, partials = false" {
            val windowed = WindowedLazySequence(source, 1, 2, false)
            windowed.remainingToList().get() shouldBe listOf(
                listOf(1),
                listOf(3),
                listOf(5),
                listOf(7),
                listOf(9),
            )
        }
    }

    "size = 3" - {
        "step = 1" - {
            "partials = false" {
                val windowed = WindowedLazySequence(source, 3, 1, false)
                windowed.remainingToList().get() shouldBe listOf(
                    listOf(1, 2, 3),
                    listOf(2, 3, 4),
                    listOf(3, 4, 5),
                    listOf(4, 5, 6),
                    listOf(5, 6, 7),
                    listOf(6, 7, 8),
                    listOf(7, 8, 9),
                )
            }

            "partials = true" {
                val windowed = WindowedLazySequence(source, 3, 1, true)
                windowed.remainingToList().get() shouldBe listOf(
                    listOf(1, 2, 3),
                    listOf(2, 3, 4),
                    listOf(3, 4, 5),
                    listOf(4, 5, 6),
                    listOf(5, 6, 7),
                    listOf(6, 7, 8),
                    listOf(7, 8, 9),
                    listOf(8, 9),
                    listOf(9),
                )
            }
        }

        "step = 2" - {
            "partials = false" {
                val windowed = WindowedLazySequence(source, 3, 2, false)
                windowed.remainingToList().get() shouldBe listOf(
                    listOf(1, 2, 3),
                    listOf(3, 4, 5),
                    listOf(5, 6, 7),
                    listOf(7, 8, 9),
                )
            }

            "partials = true" {
                val windowed = WindowedLazySequence(source, 3, 2, true)
                windowed.remainingToList().get() shouldBe listOf(
                    listOf(1, 2, 3),
                    listOf(3, 4, 5),
                    listOf(5, 6, 7),
                    listOf(7, 8, 9),
                    listOf(9),
                )
            }
        }
    }
}) {
    override fun isolationMode() = IsolationMode.InstancePerTest
}