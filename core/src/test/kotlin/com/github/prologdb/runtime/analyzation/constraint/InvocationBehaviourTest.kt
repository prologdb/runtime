package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.CompoundBuilder
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.RandomVariable
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.matchers.beInstanceOf
import io.kotlintest.matchers.should
import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.matchers.shouldNotBe
import io.kotlintest.specs.FreeSpec
import io.mockk.every
import io.mockk.mockk

class InvocationBehaviourTest : FreeSpec() {init {
    val a = CompoundBuilder("a")
    val p = CompoundBuilder("p")

    val constraint = mockk<TermConstraint>()
    every { constraint.toString(any()) } returns "constraint"

    "translate" - {
        "a([A|B], B) translated to p(A, B) results in a([Random|B], B)" {
            val A = Variable("A")
            val B = Variable("B")
            val behaviour = InvocationBehaviour(
                a(PrologList(listOf(A), B), B),
                mapOf(
                    A to TypeTermConstraint<PrologNumber>()
                ),
                listOf(mapOf(
                    A to TypeTermConstraint<PrologInteger>(),
                    B to TypeTermConstraint<PrologList>()
                ))
            )
            val translated = behaviour.translate(p(A, B), RandomVariableScope())
            translated shouldNotBe null
            translated!!

            translated.targetCallableHead.arity shouldBe 2
            translated.targetCallableHead.arguments[0] should beInstanceOf(PrologList::class)
            (translated.targetCallableHead.arguments[0] as PrologList).elements.size shouldBe 1
            val randomVar = (translated.targetCallableHead.arguments[0] as PrologList).elements[0]
            randomVar as RandomVariable
            (translated.targetCallableHead.arguments[0] as PrologList).tail shouldBe B

            translated.inConstraints.size shouldBe 1
            (randomVar in translated.inConstraints) shouldBe true
            translated.inConstraints[randomVar] shouldEqual TypeTermConstraint<PrologNumber>()
        }
    }
}
    override val oneInstancePerTest = true
}