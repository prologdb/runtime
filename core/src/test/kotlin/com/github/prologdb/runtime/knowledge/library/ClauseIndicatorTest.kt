package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.ClauseIndicator
import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldThrow
import io.kotlintest.specs.FreeSpec

class ClauseIndicatorTest : FreeSpec({
    "parse" - {
        "valid" {
            ClauseIndicator.parse("foo/1") shouldBe ClauseIndicator.of("foo", 1)
        }

        "no arity" {
            shouldThrow<IllegalArgumentException> {
                ClauseIndicator.parse("foo")
            }
        }

        "no functor" {
            shouldThrow<IllegalArgumentException> {
                ClauseIndicator.parse("2")
            }
        }

        "slash but no arity" {
            shouldThrow<IllegalArgumentException> {
                ClauseIndicator.parse("foo/")
            }
        }

        "slash but no functor" {
            shouldThrow<IllegalArgumentException> {
                ClauseIndicator.parse("/2")
            }
        }

        "arity not numeric" {
            shouldThrow<IllegalArgumentException> {
                ClauseIndicator.parse("foo/bar")
            }
        }
    }
})