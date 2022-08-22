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

class VariableBucketTest : FreeSpec({
    "variable bucket topological sort (sortForSubstitution)" - {
        "happy path" {
            // SETUP
            val bucket = VariableBucket()
            bucket.instantiate(Variable("C"), Atom("a"))
            bucket.instantiate(Variable("B"), Variable("C"))
            bucket.instantiate(Variable("A"), Variable("B"))

            // if this is the case then the variables are, by accident, sorted correctly
            // switching the names should be able to deterministically resolve this
            bucket.values.first() shouldNotBe Pair(Variable("A"), Variable("B"))

            // ACT
            val sorted = bucket.sortForSubstitution()

            var value = CompoundTerm("foo", arrayOf(Variable("A")))
            sorted.forEach { replacement ->
                value = value.substituteVariables(replacement.asSubstitutionMapper())
            }

            value shouldBe CompoundTerm("foo", arrayOf(Atom("a")))
        }

        "circular dependency should error" {
            // SETUP
            val bucket = VariableBucket()
            bucket.instantiate(Variable("A"), CompoundTerm("foo", arrayOf(Variable("B"))))
            bucket.instantiate(Variable("B"), CompoundTerm("bar", arrayOf(Variable("A"))))

            // ACT & ASSERT
            shouldThrow<CircularTermException> {
                bucket.sortForSubstitution()
            }
        }
    }

    "incorporate" - {
        "should unify" {
            val bucketA = VariableBucket().apply {
                instantiate(Variable("A"), PrologList(listOf(Variable("NestedA"))))
                instantiate(Variable("B"), Atom("ground"))
            }
            val bucketB = VariableBucket().apply {
                instantiate(Variable("A"), PrologList(listOf(Variable("NestedB"))))
                instantiate(Variable("B"), Atom("ground"))
            }

            val result = bucketA.combinedWith(bucketB, RandomVariableScope())
            result.variables should haveSize(3)
            result.values.toList().forOne { (variable, value) ->
                variable shouldBe Variable("A")
                value shouldBe PrologList(listOf(Variable("NestedA")))
            }
            result.values.toList().forOne { (variable, value) ->
                variable shouldBe Variable("B")
                value shouldBe Atom("ground")
            }
            result.values.toList().forOne { (variable, value) ->
                variable shouldBe Variable("NestedA")
                value shouldBe Variable("NestedB")
            }
        }
    }
})