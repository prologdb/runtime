package com.github.prologdb.runtime.term

import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe
import org.junit.jupiter.api.assertThrows

class PrologNumberTest : FreeSpec({
    "ULong constructor" - {
        "with value < Long.MAX_VALUE" {
            PrologNumber(2151231364.toULong()).toString() shouldBe "2151231364"
        }
        "with value > Long.MAX_VALUE but << ULong.MAX_VALUE" {
            PrologNumber(Long.MAX_VALUE.toULong() + 10000.toULong()).toString() shouldBe "9223372036854785807"
        }
        "with ULong.MAX_VALUE" {
            PrologNumber(ULong.MAX_VALUE).toString() shouldBe "18446744073709551615"
        }
    }

    "string constructor" - {
        "integer within long range" {
            PrologNumber("2151231364") shouldBe PrologNumber(2151231364)
        }

        "integer outside long range" {
            PrologNumber("9223372036854785807").toString() shouldBe "9223372036854785807"
        }

        "decimal" {
            PrologNumber("9223372036854.521").toString() shouldBe "9223372036854.521"
        }

        "invalid" {
            assertThrows<NumberFormatException> {
                PrologNumber("not a number")
            }
        }
    }
})