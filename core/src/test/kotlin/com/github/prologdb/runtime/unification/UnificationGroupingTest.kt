package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import io.kotest.core.spec.style.FreeSpec
import io.kotest.inspectors.forOne
import io.kotest.matchers.shouldBe

class UnificationGroupingTest : FreeSpec({
    "should differentiate unbound from bound" - {
        val keyUnbound = VariableBucket().apply {
            instantiate(Variable("A"), CompoundTerm("wrap", arrayOf(Variable("B"))))
        }
        val keyBound = VariableBucket().apply {
            instantiate(Variable("A"), CompoundTerm("wrap", arrayOf(Atom("ground"))))
        }

        val grouping = UnificationGrouping<String>(RandomVariableScope())
        grouping[keyUnbound] = "unbound"
        grouping[keyBound] = "bound"

        "size" {
            grouping.size shouldBe 2
        }

        "get" {
            grouping[keyUnbound] shouldBe "unbound"
            grouping[keyBound] shouldBe "bound"
        }

        "iterate" {
            grouping.toList().forOne { (vars, value) ->
                vars shouldBe keyUnbound
                value shouldBe "unbound"
            }
            grouping.toList().forOne { (vars, value) ->
                vars shouldBe keyBound
                value shouldBe "bound"
            }
        }
    }

    "should combine unbound with different variables" - {
        val keyA = VariableBucket().apply {
            instantiate(Variable("A"), CompoundTerm("wrap", arrayOf(Variable("UnboundA"))))
        }
        val keyB = VariableBucket().apply {
            instantiate(Variable("A"), CompoundTerm("wrap", arrayOf(Variable("UnboundB"))))
        }

        val grouping = UnificationGrouping<String>(RandomVariableScope())
        grouping[keyA] = "first value"
        grouping[keyB] = "second value"

        "size" {
            grouping.size shouldBe 1
        }

        "get" {
            grouping[keyA] shouldBe "second value"
            grouping[keyB] shouldBe "second value"
        }

        "iterate" {
            grouping.toList().forOne { (vars, value) ->
                vars shouldBe keyA
                value shouldBe "second value"
            }
        }
    }
})