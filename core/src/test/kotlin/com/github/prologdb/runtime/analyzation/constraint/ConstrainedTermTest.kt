package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.beAnEmptyMap
import com.github.prologdb.runtime.beMapOfSize
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundBuilder
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.RandomVariable
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.matchers.beInstanceOf
import io.kotlintest.matchers.haveKey
import io.kotlintest.matchers.should
import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldNotBe
import io.kotlintest.specs.FreeSpec
import io.mockk.every
import io.mockk.mockk

class ConstrainedTermTest : FreeSpec() {init {
    val a = CompoundBuilder("a")
    val foo = CompoundBuilder("foo")
    val bar = Atom("bar")
    val Variable = Variable("Variable")
    val Variable2 = Variable("Variable2")
    val value = Atom("value")

    val constraint = mockk<TermConstraint>()
    every { constraint.toString(any()) } returns "constraint"

    "Variable = value" - {
        val lhs = ConstrainedTerm(Variable, mapOf(Variable to constraint))
        val rhs = ConstrainedTerm(value, emptyMap())

        "with passing constraint on Variable" {
            every { constraint.check(value) } returns true
            val result = lhs.combineWith(rhs, RandomVariableScope())
            result shouldNotBe null
            result!!.structure shouldBe value
            result.constraints should beAnEmptyMap()
        }

        "with failing constraint on Variable" {
            every { constraint.check(value) } returns false
            val result = lhs.combineWith(rhs, RandomVariableScope())
            result shouldBe null
        }
    }

    "Variable = Variable2" - {
        val constraint2 = mockk<TermConstraint>()
        every { constraint2.toString(any()) } returns "constraint2"
        val lhs = ConstrainedTerm(Variable, mapOf(Variable to constraint))
        val rhs = ConstrainedTerm(Variable2, mapOf(Variable2 to constraint2))
        val combinedConstraint = mockk<TermConstraint>()
        every { combinedConstraint.toString(any()) } returns "combinedConstraint"

        "should combine constraints" {
            every { constraint.and(constraint2) } returns combinedConstraint

            val result = lhs.combineWith(rhs, RandomVariableScope())
            result shouldNotBe null
            result!!.structure shouldBe Variable2
            result.constraints should beMapOfSize(1)
            result.constraints should haveKey(Variable2)
            result.constraints[Variable2] shouldBe combinedConstraint
        }
    }

    "a(Variable) = b(Variable2)" - {
        val constraint2 = mockk<TermConstraint>()
        every { constraint2.toString(any()) } returns "constraint2"
        val lhs = ConstrainedTerm(a(Variable), mapOf(Variable to constraint))
        val rhs = ConstrainedTerm(a(Variable2), mapOf(Variable2 to constraint2))
        val combinedConstraint = mockk<TermConstraint>()
        every { combinedConstraint.toString(any()) } returns "combinedConstraint"

        "should combine constraints" {
            every { constraint.and(constraint2) } returns combinedConstraint

            val result = lhs.combineWith(rhs, RandomVariableScope())
            result shouldNotBe null
            result!!.structure shouldBe a(Variable2)
            result.constraints should beMapOfSize(1)
            result.constraints should haveKey(Variable2)
            result.constraints[Variable2] shouldBe combinedConstraint
        }
    }

    "a(Variable) = a(Variable)" - {
        val constraint2 = mockk<TermConstraint>()
        every { constraint2.toString(any()) } returns "constraint2"
        val lhs = ConstrainedTerm(a(Variable), mapOf(Variable to constraint))
        val rhs = ConstrainedTerm(a(Variable), mapOf(Variable to constraint2))
        val combinedConstraint = mockk<TermConstraint>()
        every { combinedConstraint.toString(any()) } returns "combinedConstraint"

        "should combine constraints" {
            every { constraint.and(constraint2) } returns combinedConstraint

            val result = lhs.combineWith(rhs, RandomVariableScope())
            result shouldNotBe null
            result!!.structure shouldBe a(Variable)
            result.constraints should beMapOfSize(1)
            result.constraints should haveKey(Variable)
            result.constraints[Variable] shouldBe combinedConstraint
        }
    }

    "a(Variable, foo(bar)) = a(foo(bar), Variable2)" - {
        val lhs = ConstrainedTerm(a(Variable, foo(bar)), emptyMap())
        val rhs = ConstrainedTerm(a(foo(bar), Variable2), emptyMap())

        "should inline" {
            val result = lhs.combineWith(rhs, RandomVariableScope())

            result shouldNotBe null
            result!!.structure shouldBe a(foo(bar), foo(bar))
        }
    }

    "(a(Variable, Variable2), Variable = bar) = (a(Variable, Variable2), Variable2 = value)" - {
        val lhs = ConstrainedTerm(a(Variable, Variable2), mapOf(Variable to IdentityTermConstraint(bar)))
        val rhs = ConstrainedTerm(a(Variable, Variable2), mapOf(Variable2 to IdentityTermConstraint(value)))

        "should inline identity term constraints" {
            val result = lhs.combineWith(rhs, RandomVariableScope())

            result shouldNotBe null
            result!!.structure shouldBe a(bar, value)
            result.constraints should beAnEmptyMap()
        }
    }

    "translate" - {
        "a([A|B], B) translated to p(A, B) results in a([Random|B], B)" {
            val A = Variable("A")
            val B = Variable("B")
            val term = ConstrainedTerm(a(PrologList(listOf(A), B), B), emptyMap())
            val translated = term.translate(a(A, B), RandomVariableScope())?.structure

            translated as CompoundTerm
            translated.arity shouldBe 2
            translated.arguments[0] should beInstanceOf(PrologList::class)
            (translated.arguments[0] as PrologList).elements.size shouldBe 1
            (translated.arguments[0] as PrologList).elements[0] should beInstanceOf(RandomVariable::class)
            (translated.arguments[0] as PrologList).tail shouldBe B
        }
    }
}
    override val oneInstancePerTest = true
}