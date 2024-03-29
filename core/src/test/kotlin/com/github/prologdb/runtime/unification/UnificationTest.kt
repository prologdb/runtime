package com.github.prologdb.runtime.unification


import com.github.prologdb.runtime.CircularTermException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FreeSpec
import io.kotest.inspectors.forOne
import io.kotest.matchers.collections.haveSize
import io.kotest.matchers.should
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe

class UnificationTest : FreeSpec({
    "variable bucket topological sort (sortForSubstitution)" - {
        "happy path" {
            // SETUP
            val bucket = Unification.fromMap(mapOf(
                Variable("A") to Atom("a"),
                Variable("B") to Variable("A"),
                Variable("C") to Variable("B"),
            ))

            // if this is the case then the variables are, by accident, sorted correctly
            // switching the names should be able to deterministically resolve this
            bucket.entries.first() shouldNotBe Pair(Variable("C"), Variable("B"))

            // ACT
            val sorted = bucket.sortedForSubstitution()

            var value = CompoundTerm("foo", arrayOf(Variable("A")))
            sorted.forEach { replacement ->
                value = value.substituteVariables(replacement.asSubstitutionMapper())
            }

            value shouldBe CompoundTerm("foo", arrayOf(Atom("a")))
        }

        "circular dependency should error" {
            // SETUP
            val bucket = Unification.fromMap(mapOf(
                Variable("A") to CompoundTerm("foo", arrayOf(Variable("B"))),
                Variable("B") to CompoundTerm("bar", arrayOf(Variable("A"))),
            ))

            // ACT & ASSERT
            shouldThrow<CircularTermException> {
                bucket.sortedForSubstitution()
            }
        }
    }

    "incorporate" - {
        "should unify" {
            val bucketA = Unification.fromMap(mapOf(
                Variable("A") to PrologList(listOf(Variable("NestedA"))),
                Variable("B") to Atom("ground"),
            ))
            val bucketB = Unification.fromMap(mapOf(
                Variable("A") to PrologList(listOf(Variable("NestedB"))),
                Variable("B") to Atom("ground"),
            ))

            val result = bucketA.combinedWithExpectSuccess(bucketB, RandomVariableScope())
            result.variables should haveSize(3)
            result.entries.toList().forOne { (variable, value) ->
                variable shouldBe Variable("A")
                value shouldBe PrologList(listOf(Variable("NestedA")))
            }
            result.entries.toList().forOne { (variable, value) ->
                variable shouldBe Variable("B")
                value shouldBe Atom("ground")
            }
            result.entries.toList().forOne { (variable, value) ->
                variable shouldBe Variable("NestedA")
                value shouldBe Variable("NestedB")
            }
        }
    }

    "compact" - {
        "aggressive" - {
            "common value being a variable" - {
                "one way" {
                    val bucket = Unification.fromMap(
                        mapOf(
                            Variable("A") to Variable("X"),
                            Variable("B") to Variable("X"),
                            Variable("C") to Variable("X"),
                        )
                    )

                    val compacted = bucket.compacted(true)
                    compacted.variables should haveSize(2)
                    compacted[Variable("A")] shouldBe Variable("B")
                    compacted[Variable("C")] shouldBe Variable("B")
                }

                "two way" {
                    val bucket = Unification.fromMap(
                        mapOf(
                            Variable("A") to Variable("X"),
                            Variable("B") to Variable("X"),
                            Variable("C") to Variable("X"),
                            Variable("X") to Variable("D"),
                        )
                    )

                    val compacted = bucket.compacted(true)
                    compacted.variables should haveSize(3)
                    compacted[Variable("B")] shouldBe Variable("A")
                    compacted[Variable("C")] shouldBe Variable("A")
                    compacted[Variable("X")] shouldBe Variable("A")
                }
            }

            "common value being a nonvar" - {
                "one way" {
                    val bucket = Unification.fromMap(
                        mapOf(
                            Variable("A") to Atom("a"),
                            Variable("B") to Atom("a"),
                            Variable("C") to Atom("a"),
                        )
                    )

                    val compacted = bucket.compacted(true)
                    compacted.variables should haveSize(3)
                    compacted[Variable("A")] shouldBe Atom("a")
                    compacted[Variable("B")] shouldBe Variable("A")
                    compacted[Variable("C")] shouldBe Variable("A")
                }

                "one way with additional indirection" {
                    val bucket = Unification.fromMap(
                        mapOf(
                            Variable("A") to Atom("a"),
                            Variable("B") to Atom("a"),
                            Variable("C") to Atom("a"),
                            Variable("D") to Variable("C"),
                        )
                    )

                    val compacted = bucket.compacted(true)
                    compacted.variables should haveSize(4)
                    compacted[Variable("A")] shouldBe Atom("a")
                    compacted[Variable("B")] shouldBe Variable("A")
                    compacted[Variable("C")] shouldBe Variable("A")
                    compacted[Variable("D")] shouldBe Variable("A")
                }

                "two way indirect" {
                    val bucket = Unification.fromMap(
                        mapOf(
                            Variable("A") to Variable("X"),
                            Variable("B") to Variable("X"),
                            Variable("C") to Variable("X"),
                            Variable("X") to Variable("D"),
                        )
                    )

                    val compacted = bucket.compacted(true)
                    compacted.variables should haveSize(3)
                    compacted[Variable("B")] shouldBe Variable("A")
                    compacted[Variable("C")] shouldBe Variable("A")
                    compacted[Variable("X")] shouldBe Variable("A")
                }
            }
        }
    }
})