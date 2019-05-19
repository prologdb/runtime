package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.module.*
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundBuilder
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.specs.FreeSpec

class ModuleIsolationTest : FreeSpec({
    val a = Atom("a")
    val b = Atom("b")
    val X = Variable("X")
    val R = Variable("R")
    val foo = CompoundBuilder("foo")
    val bar = CompoundBuilder("bar")

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
            dynamicPredicates = emptySet()
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
            dynamicPredicates = emptySet()
        )
        val moduleARef = ModuleReference("module", "A")

        val moduleLoader = NativeLibraryLoader()
        moduleLoader.registerModule("module", moduleA)

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                FullModuleImport(moduleARef)
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet()
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
            dynamicPredicates = emptySet()
        )
        val moduleARef = ModuleReference("module", "A")

        val moduleLoader = NativeLibraryLoader()
        moduleLoader.registerModule("module", moduleA)

        val moduleB = ASTModule(
            name = "B",
            imports = listOf(
                FullModuleImport(moduleARef)
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet()
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
            dynamicPredicates = emptySet()
        )
        val moduleARef = ModuleReference("module", "A")

        val moduleLoader = NativeLibraryLoader()
        moduleLoader.registerModule("module", moduleA)

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
            dynamicPredicates = emptySet()
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

    "module local predicates" - {
        val delegateA = CompoundBuilder("delegateA")
        val delegateB = CompoundBuilder("delegateB")
        val clauseFooA = foo(a)
        val clauseFooB = foo(b)

        val clauseDelegateAToFoo = Rule(
            delegateA(X),
            PredicateInvocationQuery(foo(X))
        )
        val clauseDelegateBToFoo = Rule(
            delegateB(X),
            PredicateInvocationQuery(foo(X))
        )

        val moduleA = ASTModule(
            name = "A",
            imports = emptyList(),
            givenClauses = listOf(clauseFooA, clauseDelegateAToFoo),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseDelegateAToFoo)),
            dynamicPredicates = emptySet()
        )
        val moduleARef = ModuleReference("module", "A")

        val moduleB = ASTModule(
            name = "B",
            imports = emptyList(),
            givenClauses = listOf(clauseFooB, clauseDelegateBToFoo),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseDelegateBToFoo)),
            dynamicPredicates = emptySet()
        )
        val moduleBRef = ModuleReference("module", "B")

        /* in short:
        :- module(`A`, [delegateA/1]).
        foo(a).
        delegateA(X) :- foo(X).

        :- module(`B`, [delegateB/1]).
        foo(b).
        delegateB(X) :- foo(X).
         */

        // 1: from within module A foo(R) yields R = a.
        // 2: from within module B foo(R) yields R = b.
        // 3: from within root     delegateA(R) yields R = a.
        // 3: from within root     delegateB(R) yields R = b.


        "when loaded into same runtime" - {
            val moduleLoader = NativeLibraryLoader().apply {
                registerModule("module", moduleA)
                registerModule("module", moduleB)
            }

            "from within module" {
                val runtimeEnv = PrologRuntimeEnvironment(
                    ASTModule(
                        name = "__root",
                        imports = listOf(FullModuleImport(moduleARef), FullModuleImport(moduleBRef)),
                        givenClauses = emptyList(),
                        dynamicPredicates = emptySet(),
                        exportedPredicateIndicators = emptySet()
                    ),
                    moduleLoader
                )

                moduleA.shouldProveWithinRuntime(runtimeEnv, foo(R)) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = a") {
                        it.variableValues[R] == a
                    }
                }

                moduleB.shouldProveWithinRuntime(runtimeEnv, foo(R)) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = b") {
                        it.variableValues[R] == b
                    }
                }
            }

            "from root module via exported delegate" {
                val runtimeEnv = PrologRuntimeEnvironment(
                    ASTModule(
                        name = "__root",
                        imports = listOf(
                            FullModuleImport(moduleARef),
                            FullModuleImport(moduleBRef)
                        ),
                        givenClauses = emptyList(),
                        dynamicPredicates = emptySet(),
                        exportedPredicateIndicators = emptySet()
                    ),
                    moduleLoader
                )

                runtimeEnv shouldProve delegateA(R) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = a") {
                        it.variableValues[R] == a
                    }
                }

                runtimeEnv shouldProve delegateB(R) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = b") {
                        it.variableValues[R] == b
                    }
                }
            }
        }
    }

    "export with rename" {
        val clauseFooA = foo(a)

        val moduleA = ASTModule(
            name = "a",
            imports = emptyList(),
            givenClauses = listOf(clauseFooA),
            dynamicPredicates = emptySet(),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseFooA))
        )

        val moduleB = ASTModule(
            name = "b",
            imports = listOf(SelectiveModuleImport(
                ModuleReference("module", "a"),
                mapOf(
                    ClauseIndicator.of("foo", 1) to "bar"
                )
            )),
            givenClauses = emptyList(),
            dynamicPredicates = emptySet(),
            exportedPredicateIndicators = emptySet()
        )

        val loader = NativeLibraryLoader().apply {
            registerModule("module", moduleA)
            registerModule("module", moduleB)
        }

        val runtimeEnv = PrologRuntimeEnvironment(moduleB, loader)

        runtimeEnv shouldProve bar(X) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X = a") {
                it.variableValues[X] == a
            }
        }
    }
}) {
    override val oneInstancePerTest = true
}
