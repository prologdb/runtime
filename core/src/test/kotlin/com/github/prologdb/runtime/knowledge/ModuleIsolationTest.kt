package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PredicateNotDefinedException
import com.github.prologdb.runtime.module.ASTModule
import com.github.prologdb.runtime.module.ModuleDeclaration
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.runtimeWithLoadedModules
import com.github.prologdb.runtime.shouldProve
import com.github.prologdb.runtime.shouldProveInContextOfModule
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundBuilder
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.IsolationMode
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

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
            declaration = ModuleDeclaration("user"),
            imports = emptyList(),
            givenClauses = listOf(clauseA01, clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )

        runtimeWithLoadedModules(module) shouldProve foo(R) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it[R] == a
            }
        }
    }

    "full import - predicates in module see imported predicates" {
        val moduleA = ASTModule(
            declaration = ModuleDeclaration("A"),
            imports = emptyList(),
            givenClauses = listOf(clauseA01),
            exportedPredicateIndicators = setOf(clauseA01Indicator),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),

        )
        val moduleARef = ModuleReference("module", "A")

        val userModule = ASTModule(
            declaration = ModuleDeclaration("user"),
            imports = listOf(
                ModuleImport.Full(moduleARef)
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )

        runtimeWithLoadedModules(moduleA, userModule) shouldProve foo(R) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it[R] == a
            }
        }
    }

    "full import - predicates in module don't see predicates private to imported module" {
        val moduleA = ASTModule(
            declaration = ModuleDeclaration("A"),
            imports = emptyList(),
            givenClauses = listOf(clauseA01),
            exportedPredicateIndicators = emptySet(), // a(a) is private
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )
        val moduleARef = ModuleReference("module", "A")

        val userModule = ASTModule(
            declaration = ModuleDeclaration("user"),
            imports = listOf(
                ModuleImport.Full(moduleARef)
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )

        val ex = shouldThrow<PredicateNotDefinedException> {
            runtimeWithLoadedModules(moduleA, userModule).fulfill("user", PredicateInvocationQuery(foo(R))).consumeAll().get()
        }
        ex.message shouldBe "Predicate a/1 not defined in context of module user"
    }

    "partial import - predicates in module don't see predicates not imported (but exported)" {
        val clauseC01 = CompoundTerm("c", arrayOf(Atom("c")))

        val moduleA = ASTModule(
            declaration = ModuleDeclaration("A"),
            imports = emptyList(),
            givenClauses = listOf(clauseA01, clauseC01),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseA01), ClauseIndicator.of(clauseC01)),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )
        val moduleARef = ModuleReference("module", "A")

        val userModule = ASTModule(
            declaration = ModuleDeclaration("user"),
            imports = listOf(
                ModuleImport.Selective(
                    moduleARef,
                    mapOf(
                        ClauseIndicator.of(clauseA01) to clauseA01.functor
                        // c/1 is not imported
                    ),
                    emptySet(),
                )
            ),
            givenClauses = listOf(clauseFoo01, clauseBar01),
            exportedPredicateIndicators = setOf(clauseFoo01Indicator),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )

        val runtimeEnv = runtimeWithLoadedModules(moduleA, userModule)

        runtimeEnv shouldProve foo(R) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = a") {
                it[R] == a
            }
        }

        val ex = shouldThrow<PredicateNotDefinedException> {
            runtimeEnv.fulfill("user", PredicateInvocationQuery(CompoundTerm("bar", arrayOf(R)))).consumeAll().get()
        }
        ex.message shouldBe "Predicate c/1 not defined in context of module user"
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
            declaration = ModuleDeclaration("A"),
            imports = emptyList(),
            givenClauses = listOf(clauseFooA, clauseDelegateAToFoo),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseDelegateAToFoo)),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )
        val moduleARef = ModuleReference("module", "A")

        val moduleB = ASTModule(
            declaration = ModuleDeclaration("B"),
            imports = emptyList(),
            givenClauses = listOf(clauseFooB, clauseDelegateBToFoo),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseDelegateBToFoo)),
            dynamicPredicates = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
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
        // 3: from within user     delegateA(R) yields R = a.
        // 3: from within user     delegateB(R) yields R = b.


        "when loaded into same runtime" - {
            "from within module" {
                val userModule = ASTModule(
                    ModuleDeclaration("user"),
                    emptyList(),
                    emptyList(),
                    emptySet(),
                    emptySet(),
                    emptySet(),
                    emptySet(),
                    emptySet(),
                )

                val runtimeEnv = runtimeWithLoadedModules(moduleA, moduleB, userModule)

                runtimeEnv.shouldProveInContextOfModule(moduleA.declaration.moduleName, foo(R)) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = a") {
                        it[R] == a
                    }
                }

                runtimeEnv.shouldProveInContextOfModule(moduleB.declaration.moduleName, foo(R)) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = b") {
                        it[R] == b
                    }
                }
            }

            "from root module via exported delegate" {
                val userModule = ASTModule(
                    ModuleDeclaration("user"),
                    listOf(
                        ModuleImport.Full(moduleARef),
                        ModuleImport.Full(moduleBRef)
                    ),
                    emptyList(),
                    emptySet(),
                    emptySet(),
                    emptySet(),
                    emptySet(),
                    emptySet(),
                )

                val runtimeEnv = runtimeWithLoadedModules(moduleA, moduleB, userModule)

                runtimeEnv shouldProve delegateA(R) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = a") {
                        it[R] == a
                    }
                }

                runtimeEnv shouldProve delegateB(R) suchThat {
                    itHasExactlyOneSolution()
                    itHasASolutionSuchThat("R = b") {
                        it[R] == b
                    }
                }
            }
        }
    }

    "export with rename" {
        val clauseFooA = foo(a)

        val moduleA = ASTModule(
            declaration = ModuleDeclaration("A"),
            imports = emptyList(),
            givenClauses = listOf(clauseFooA),
            dynamicPredicates = emptySet(),
            exportedPredicateIndicators = setOf(ClauseIndicator.of(clauseFooA)),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )

        val userModule = ASTModule(
            declaration = ModuleDeclaration("user"),
            imports = listOf(ModuleImport.Selective(
                ModuleReference("module", "A"),
                mapOf(
                    ClauseIndicator.of("foo", 1) to "bar"
                ),
                emptySet(),
            )),
            givenClauses = emptyList(),
            dynamicPredicates = emptySet(),
            exportedPredicateIndicators = emptySet(),
            moduleTransparents = emptySet(),
            deterministics = emptySet(),
            semiDeterministics = emptySet(),
        )

        val runtime = runtimeWithLoadedModules(moduleA, userModule)

        runtime shouldProve bar(X) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X = a") {
                it[X] == a
            }
        }
    }
}) {
    override fun isolationMode() = IsolationMode.InstancePerTest
}
