package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.knowledge.library.ASTModule
import com.github.prologdb.runtime.knowledge.library.ClauseIndicator
import com.github.prologdb.runtime.knowledge.library.EmptyOperatorRegistry
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import io.kotlintest.matchers.shouldNotBe
import io.kotlintest.specs.FreeSpec

class ModuleIsolationTest : FreeSpec({
    "clauses in module see other declarations of same module" {
        val a = Atom("a")
        val X = Variable("X")
        val R = Variable("R")

        val clauseA = CompoundTerm("a", arrayOf(a))
        val clauseB = Rule(
            CompoundTerm("foo", arrayOf(X)),
            AndQuery(arrayOf(
                PredicateInvocationQuery(CompoundTerm("a", arrayOf(X)))
            ))
        )
        val clauseBIndicator = ClauseIndicator.of(clauseB)

        val module = ASTModule(
            name = "test",
            imports = emptyList(),
            givenClauses = listOf(clauseA, clauseB),
            exportedPredicateIndicators = setOf(clauseBIndicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        module.exportedPredicates[clauseBIndicator] shouldNotBe null

        val results = {
            val psc = PrologRuntimeEnvironment().newProofSearchContext()
            buildLazySequence<Unification>(psc.principal) {
                module.exportedPredicates.getValue(clauseBIndicator).fulfill(
                    this@buildLazySequence,
                    CompoundTerm("foo", arrayOf(R)),
                    psc
                )
            }
        }

        results.suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it.variableValues[R] == a
            }
        }
    }
})