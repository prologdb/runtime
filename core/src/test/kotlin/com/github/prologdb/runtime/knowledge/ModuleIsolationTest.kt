package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.shouldNotProve
import com.github.prologdb.runtime.shouldProve
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundBuilder
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.specs.FreeSpec

class ModuleIsolationTest : FreeSpec({
    val a = Atom("a")
    val X = Variable("X")
    val R = Variable("R")
    val foo = CompoundBuilder("foo")

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

        PrologRuntimeEnvironment(module) shouldProve foo(R) suchThat {
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
        val moduleARef = ModuleReference("module", "A")

        val moduleLoader = NativeLibraryLoader()
        moduleLoader.registerModule(moduleARef, moduleA)

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                FullModuleImport(moduleARef)
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        val runtimeEnv = PrologRuntimeEnvironment(moduleB, moduleLoader)

        runtimeEnv shouldProve foo(R) suchThat {
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
        val moduleARef = ModuleReference("module", "A")

        val moduleLoader = NativeLibraryLoader()
        moduleLoader.registerModule(moduleARef, moduleA)

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                FullModuleImport(moduleARef)
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        val runtimeEnv = PrologRuntimeEnvironment(moduleB, moduleLoader)

        runtimeEnv shouldNotProve foo(R)
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
        val moduleARef = ModuleReference("module", "A")

        val moduleLoader = NativeLibraryLoader()
        moduleLoader.registerModule(moduleARef, moduleA)

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                SelectiveModuleImport(moduleARef, mapOf(
                    ClauseIndicator.of(clauseA01) to clauseA01.functor
                    // c/1 is not imported
                ))
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            exportedOperators = EmptyOperatorRegistry
        )

        val runtimeEnv = PrologRuntimeEnvironment(moduleB, moduleLoader)

        runtimeEnv shouldProve foo(R) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it.variableValues[R] == a
            }
        }

        runtimeEnv shouldNotProve CompoundTerm("bar", arrayOf(R))
    }
}) {
    override val oneInstancePerTest = true
}
