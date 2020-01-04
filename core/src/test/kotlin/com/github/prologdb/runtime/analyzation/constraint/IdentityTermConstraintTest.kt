package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologNumber
import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.FreeSpec

class IdentityTermConstraintTest : FreeSpec() { init {
    "check" - {
        "with same type" {
            IdentityTermConstraint(PrologInteger(4)).check(PrologInteger(3)) shouldBe false
        }

        "with same term instance" {
            val term = Atom("bla")
            IdentityTermConstraint(term).check(term) shouldBe true
        }

        "with equal terms of different identity" {
            val termA = Atom("foo")
            val termB = Atom("foo")
            check(termA == termB)
            check(termA !== termB)
            IdentityTermConstraint(termA).check(termB) shouldBe true
        }
    }

    "and" - {
        "noop" {
            val constraint = IdentityTermConstraint(PrologInteger(1))
            constraint.and(NoopConstraint, RandomVariableScope()) shouldBe constraint
        }

        "impossible" {
            IdentityTermConstraint(Atom("hans")).and(ImpossibleConstraint, RandomVariableScope()) shouldBe ImpossibleConstraint
        }

        "type" - {
            "the type constraints onto the same type" {
                val constraint = IdentityTermConstraint(Atom("hans"))
                constraint.and(TypeTermConstraint(Atom::class.java), RandomVariableScope()) shouldBe constraint
            }

            "the type constraints on a supertype" {
                val constraint = IdentityTermConstraint(PrologInteger(123123))
                constraint.and(TypeTermConstraint(PrologNumber::class.java), RandomVariableScope()) shouldBe constraint
            }

            "the type constraints on a disjoint type" {
                val constraint = IdentityTermConstraint(PrologDecimal(51024.0))
                constraint.and(TypeTermConstraint(PrologInteger::class.java), RandomVariableScope()) shouldBe ImpossibleConstraint
            }
        }

        "identity" - {
            "same instance" {
                val term = Atom("foo")
                val constraint = IdentityTermConstraint(term)
                constraint.and(IdentityTermConstraint(term), RandomVariableScope()) shouldBe constraint
            }

            "identical terms of different identity" {
                val termA = Atom("bla")
                val termB = Atom("bla")
                check(termA == termB)
                check(termA !== termB)

                val constraint = IdentityTermConstraint(termA)
                constraint.and(IdentityTermConstraint(termB), RandomVariableScope()) shouldBe constraint
            }

            "unequal terms" {
                IdentityTermConstraint(PrologInteger(512312)).and(IdentityTermConstraint(PrologInteger(63251)), RandomVariableScope()) shouldBe ImpossibleConstraint
            }
        }
    }
}
}