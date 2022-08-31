package com.github.prologdb.runtime.term

import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe
import org.junit.jupiter.api.assertThrows
import kotlin.random.Random

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

    "big number serialization" - {
        "just serialize" {
            val (mantissaBytes, signum, scale) = PrologBigNumber("314.15928").serialize()
            mantissaBytes shouldBe byteArrayOf(1, -33, 94, 120)
            signum shouldBe 1
            scale shouldBe -5
        }

        "just deserialize" {
            val number = PrologBigNumber(byteArrayOf(1, -33, 94, 120), 0, 4, 1, -5)
            number shouldBe PrologNumber("314.15928")
        }

        "random numbers" {
            val context = MathContext(100, MathContext.RoundingMode.HALF_UP)
            repeat(100) {
                val mantissa = Random.nextLong()
                val scale = Random.nextLong(-1000, 1000)
                val number = PrologBigNumber(mantissa.toString(10), 10)
                    .times(PrologNumber(10).toThe(PrologNumber(scale), context), context)
                number as PrologBigNumber

                val serialized = number.serialize()
                val deserialized = PrologBigNumber(serialized.first, 0, serialized.first.size, serialized.second, serialized.third)

                deserialized shouldBe number
            }
        }
    }
})