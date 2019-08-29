package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.FreeSpec

class ListConstraintTest : FreeSpec({
    val tail = Variable("T")
    val someTerm = Atom("a")
    val emptyList = PrologList(emptyList())
    val emptyListWithTail = PrologList(emptyList(), tail)
    val oneList = PrologList(listOf(someTerm))
    val oneListWithTail = PrologList(listOf(someTerm), tail)

    "nEntries = 0, moreAllowed = true" - {
        val constraint = ListConstraint(emptyList(), true)

        "empty list without tail" {
            constraint.check(emptyList) shouldBe true
        }

        "empty list with tail" {
            constraint.check(emptyListWithTail) shouldBe true
        }

        "list with one element without tail" {
            constraint.check(oneList) shouldBe true
        }

        "list with one element with tail" {
            constraint.check(oneListWithTail) shouldBe true
        }

        "atom" {
            constraint.check(someTerm) shouldBe false
        }
    }

    "nEntries = 0, more allowed = false" - {
        val constraint = ListConstraint(emptyList(), false)

        "empty list without tail" {
            constraint.check(emptyList) shouldBe true
        }

        "empty list with tail" {
            constraint.check(emptyListWithTail) shouldBe true
        }

        "list with one element without tail" {
            constraint.check(oneList) shouldBe false
        }

        "list with one element with tail" {
            constraint.check(oneListWithTail) shouldBe false
        }
    }

    "nEntries = 1, more allowed = true" - {
        val constraint = ListConstraint(listOf(NoopConstraint), true)

        "empty list without tails" {
            constraint.check(emptyList) shouldBe false
        }

        "empty list with tail" {
            constraint.check(emptyListWithTail) shouldBe false
        }

        "list with one element without tail" {
            constraint.check(oneList) shouldBe true
        }

        "list with one element with tail" {
            constraint.check(oneListWithTail) shouldBe true
        }

        "atom" {
            constraint.check(someTerm) shouldBe false
        }
    }
})