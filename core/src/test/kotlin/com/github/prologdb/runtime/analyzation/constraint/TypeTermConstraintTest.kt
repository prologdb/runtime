package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologNumber
import io.kotlintest.matchers.beInstanceOf
import io.kotlintest.matchers.should
import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.FreeSpec

class TypeTermConstraintTest : FreeSpec() {init {
    "check" - {
        "with exact type match" {
            TypeTermConstraint(PrologInteger::class.java).check(PrologInteger(5123)) shouldBe true
        }

        "with term of matching subclass" {
            TypeTermConstraint(PrologNumber::class.java).check(PrologDecimal(5131.0)) shouldBe true
        }

        "with mismatching term" {
            TypeTermConstraint(Atom::class.java).check(CompoundTerm("foo", emptyArray())) shouldBe false
        }
    }

    "and" - {
        "with noop" {
            val typeConstraint = TypeTermConstraint(Atom::class.java)
            typeConstraint.and(NoopConstraint, RandomVariableScope()) shouldBe typeConstraint
        }

        "with impossible constraint" {
            TypeTermConstraint(Atom::class.java).and(ImpossibleConstraint, RandomVariableScope()) shouldBe ImpossibleConstraint
        }

        "with type constraint" - {
            "should pick the most concrete - A" {
                val a = TypeTermConstraint(PrologNumber::class.java)
                val b = TypeTermConstraint(PrologDecimal::class.java)
                val result = a.and(b, RandomVariableScope())
                result should beInstanceOf(TypeTermConstraint::class)
                (result as TypeTermConstraint).type shouldBe PrologDecimal::class.java
            }

            "should pick the most concrete - B" {
                val a = TypeTermConstraint(PrologDecimal::class.java)
                val b = TypeTermConstraint(PrologNumber::class.java)
                val result = a.and(b, RandomVariableScope())
                result should beInstanceOf(TypeTermConstraint::class)
                (result as TypeTermConstraint).type shouldBe PrologDecimal::class.java
            }

            "should return impossible on disjoint types" {
                val a = TypeTermConstraint(Atom::class.java)
                val b = TypeTermConstraint(CompoundTerm::class.java)
                val result = a.and(b, RandomVariableScope())

                result shouldBe ImpossibleConstraint
            }
        }

        "with literal constraint" - {
            "of matching type should return the literal" {
                val literal = IdentityTermConstraint(Atom("a"))
                val type = TypeTermConstraint(Atom::class.java)
                type.and(literal, RandomVariableScope()) shouldBe literal
            }

            "of disjoint type should return Impossible" {
                val literal = IdentityTermConstraint(PrologInteger(1621))
                val type = TypeTermConstraint(Atom::class.java)
                type.and(literal, RandomVariableScope()) shouldBe ImpossibleConstraint
            }
        }
    }
}}