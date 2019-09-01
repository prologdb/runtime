package com.github.prologdb.runtime.analyzation.constraint

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
        "with matching term" {
            TypeTermConstraint(PrologNumber::class.java).check(PrologInteger(5131)) shouldBe true
        }

        "with mismatching term" {
            TypeTermConstraint(Atom::class.java).check(CompoundTerm("foo", emptyArray())) shouldBe false
        }
    }

    "and" - {
        "with type constraint" - {
            "should pick the most concrete - A" {
                val a = TypeTermConstraint(PrologNumber::class.java)
                val b = TypeTermConstraint(PrologDecimal::class.java)
                val result = a.and(b)
                result should beInstanceOf(TypeTermConstraint::class)
                (result as TypeTermConstraint).type shouldBe PrologDecimal::class.java
            }

            "should pick the most concrete - B" {
                val a = TypeTermConstraint(PrologDecimal::class.java)
                val b = TypeTermConstraint(PrologNumber::class.java)
                val result = a.and(b)
                result should beInstanceOf(TypeTermConstraint::class)
                (result as TypeTermConstraint).type shouldBe PrologDecimal::class.java
            }

            "should return impossible on disjoint types" {
                val a = TypeTermConstraint(Atom::class.java)
                val b = TypeTermConstraint(CompoundTerm::class.java)
                val result = a.and(b)

                result shouldBe ImpossibleConstraint
            }
        }

        "with literal constraint" - {
            "of matching type should return the literal" {
                val literal = IdentityTermConstraint(Atom("a"))
                val type = TypeTermConstraint(Atom::class.java)
                type.and(literal) shouldBe literal
            }

            "of disjoint type should return Impossible" {
                val literal = IdentityTermConstraint(PrologInteger(1621))
                val type = TypeTermConstraint(Atom::class.java)
                type.and(literal) shouldBe ImpossibleConstraint
            }
        }
    }
}}