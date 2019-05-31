package com.github.prologdb.runtime.optimization

import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.FreeSpec

class TailCallOptimizerTest : FreeSpec({
    "bla" {
        val clause1 = CompoundTerm("length", arrayOf(PrologList(emptyList()), PrologInteger(0)))
        val clause2 = CompoundTerm("length", arrayOf(PrologList(listOf(Variable("H")), Variable("T")), Variable("L")))

        TailcallOptimizer().areMutuallyExclusive(listOf(clause1, clause2)) shouldBe true
    }
})
