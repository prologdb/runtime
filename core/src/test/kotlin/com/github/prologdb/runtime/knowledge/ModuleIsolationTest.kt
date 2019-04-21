package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import io.kotlintest.specs.FreeSpec

class ModuleIsolationTest : FreeSpec({
    val a = Atom("a")
    val X = Variable("X")
    val R = Variable("R")

    val clauseA01 = CompoundTerm("a", arrayOf(a))
    val clauseFoo01 = Rule(
        CompoundTerm("foo", arrayOf(X)),
        AndQuery(arrayOf(
            PredicateInvocationQuery(CompoundTerm("a", arrayOf(X)))
        ))
    )

    val clauseBar01 = Rule(
        CompoundTerm("bar", arrayOf(X)),
        AndQuery(arrayOf(
            PredicateInvocationQuery(CompoundTerm("c", arrayOf(X)))
        ))
    )

    val clauseA01Indicator = ClauseIndicator.of(clauseA01)
    val clauseFoo01Indicator = ClauseIndicator.of(clauseFoo01)

    "clauses in module see other declarations of same module" {
        val module = ASTModule(
            name = "test",
            imports = emptyList(),
            givenClauses = listOf(clauseA01, clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        module shouldSeeProofFor CompoundTerm("foo", arrayOf(R)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it.variableValues[R] == a
            }
        }
    }

    "full import - predicates in module see imported predicates" {
        val moduleA = ASTModule(
            name = "A",
            imports = emptyList(),
            givenClauses = listOf(clauseA01),
            exportedPredicateIndicators = setOf(clauseA01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                FullModuleImport(ModuleReference("module", "a"))
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        moduleB shouldSeeProofFor CompoundTerm("foo", arrayOf(R)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it.variableValues[R] == a
            }
        }
    }

    "full import - predicates in module don't see predicates private to imported module" {
        val moduleA = ASTModule(
            name = "A",
            imports = emptyList(),
            givenClauses = listOf(clauseA01),
            exportedPredicateIndicators = emptySet(), // a(a) is private
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                FullModuleImport(ModuleReference("module", "A"))
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        moduleB shouldSeeProofFor CompoundTerm("foo", arrayOf(R)) suchThat {
            itHasNoSolutions()
        }
    }

    "partial import - predicates in module don't see predicates not imported (but exported)" {
        val clauseC01 = CompoundTerm("c", arrayOf(Atom("c")))

        val moduleA = ASTModule(
            name = "A",
            imports = emptyList(),
            givenClauses = listOf(clauseA01, clauseC01),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseA01), ClauseIndicator.of(clauseC01)),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                PartialModuleImport(ModuleReference("module", "A"), setOf(
                    ClauseIndicator.of(clauseA01)
                    // c/1 is not imported
                ))
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        moduleB shouldSeeProofFor CompoundTerm("foo", arrayOf(R)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it.variableValues[R] == a
            }
        }

        moduleB shouldSeeProofFor CompoundTerm("bar", arrayOf(R)) suchThat {
            itHasNoSolutions()
        }
    }
}) {
    override val oneInstancePerTest = true
}

private infix fun Module.shouldSeeProofFor(query: CompoundTerm): () -> LazySequence<Unification> {
    return {
        val psc = PrologRuntimeEnvironment().newProofSearchContext()
        buildLazySequence<Unification>(psc.principal) {
            this@shouldSeeProofFor.exportedPredicates[ClauseIndicator.of(query)]
                ?.fulfill?.invoke(this@buildLazySequence, query, psc)
        }
    }
}
