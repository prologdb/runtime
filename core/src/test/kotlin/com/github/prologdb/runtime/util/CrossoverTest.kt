package com.github.prologdb.runtime.util

import io.kotlintest.matchers.contain
import io.kotlintest.matchers.should
import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldThrowAny
import io.kotlintest.specs.FreeSpec

class CrossoverTest : FreeSpec({
    "should throw for 0 elements" {
        shouldThrowAny {
            emptyList<Unit>().crossover({ a, b -> Unit }).toList()
        }
    }

    "should throw for 1 element" {
        shouldThrowAny {
            emptyList<Unit>().crossover({ a, b -> Unit }).toList()
        }
    }

    "should return correct result for 5 elements" {
        val crossover = listOf(
            "a",
            "b",
            "c",
            "d"
        )
            .crossover { a, b -> "$a$b" }
            .toSet()

        crossover.size shouldBe 6
        crossover should contain("ab")
        crossover should contain("ac")
        crossover should contain("ad")
        crossover should contain("bc")
        crossover should contain("bd")
        crossover should contain("cd")
    }
})
