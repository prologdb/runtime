package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.module.*
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.*
import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.FreeSpec

class DefaultPrologRuntimeEnvironmentTest : FreeSpec() {init {
    "f(a). f(b). ?- f(X)" {
        val f = CompoundBuilder("f")
        val a = Atom("a")
        val b = Atom("b")
        val runtimeEnv = runtimeWithUserClauses(
            f(a),
            f(b)
        )

        val X = Variable("X")

        runtimeEnv shouldProve f(X) suchThat {
            itHasExactlyNSolutions(2)
            itHasASolutionSuchThat("X is instantiated to a") {
                it.variableValues[X] == a
            }
            itHasASolutionSuchThat("X is instantiated to b") {
                it.variableValues[X] == b
            }
        }
    }

    "separate variable scopes: f(a(X,Y),a(Y,X)). ?-f(a(m,n),X))" {
        val f = CompoundBuilder("f")
        val a = CompoundBuilder("a")
        val m = Atom("m")
        val n = Atom("n")
        val X = Variable("X")
        val Y = Variable("Y")

        val runtimeEnv = runtimeWithUserClauses(
            f(a(X, Y), a(Y, X))
        )

        runtimeEnv shouldProve f(a(m, n), X) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X = a(n, m)") {
                it.variableValues[X] == a(n, m)
            }
        }
    }

    "lines" {
        /**
         * vertical(line(point(X,Y),point(X,Z))).
         * horizontal(line(point(X,Y),point(Z,Y))).
         * ?- vertical(line(point(a,a),point(a,c))).
         * true
         * ?- vertical(line(point(a,a),point(c,b))).
         * false
         * ?- horizontal(line(point(a,a),point(b,Y))).
         * Y = _4711
         * ?- horizontal(line(point(b,c),P)).
         * P = point(_4711, 3)
         */
        val vertical = CompoundBuilder("vertical")
        val horizontal = CompoundBuilder("horizontal")
        val line = CompoundBuilder("line")
        val point = CompoundBuilder("point")
        val a = Atom("a")
        val b = Atom("b")
        val c = Atom("c")
        val X = Variable("X")
        val Y = Variable("Y")
        val Z = Variable("Z")
        val P = Variable("P")

        val runtimeEnv = runtimeWithUserClauses(
            vertical(line(point(X, Y), point(X, Z))),
            horizontal(line(point(X,Y),point(Z,Y)))
        )

        // ASSERT
        runtimeEnv shouldProve vertical(line(point(a, a), point(a, c))) suchThat {
            itHasExactlyOneSolution()
        }

        runtimeEnv shouldNotProve vertical(line(point(a, a), point(c, b)))

        runtimeEnv shouldProve horizontal(line(point(a, a), point(b, Y))) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("Y = a") {
                it.variableValues[Y] == a
            }
        }

        runtimeEnv shouldProve horizontal(line(point(b, c), P)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("P = point(_R,c)") {
                val valP = it.variableValues[P]

                valP is CompoundTerm && valP.arguments.size == 2 && valP.arguments[0] is RandomVariable && valP.arguments[1] == c
            }
        }
    }

    "g(X, X). ?- g(A, B)" {
        val g = CompoundBuilder("g")
        val X = Variable("X")
        val A = Variable("A")
        val B = Variable("B")

        val runtimeEnv = runtimeWithUserClauses(
            g(X, X)
        )

        runtimeEnv shouldProve g(A, B) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("A = B") {
                it.variableValues[A] == B || it.variableValues[A] == it.variableValues[B]
            }
        }
    }

    "g(X, X). f(X, Y) :- g(X, Y). ?- f(a, V)" {
        val f = CompoundBuilder("f")
        val g = CompoundBuilder("g")
        val X = Variable("X")
        val Y = Variable("Y")
        val a = Atom("a")
        val V = Variable("V")

        val runtimeEnv = runtimeWithUserClauses(
            Rule(f(X, Y), PredicateInvocationQuery(g(X, Y))),
            g(X, X)
        )

        runtimeEnv shouldProve f(a, V) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("V = a") {
                it.variableValues[V] == a
            }
        }
    }

    "list append" - {
        val app = CompoundBuilder("app")
        val L = Variable("L")
        val H = Variable("H")
        val T = Variable("T")
        val L3 = Variable("L3")
        val L2 = Variable("L2")
        val R = Variable("R")

        val a = Atom("a")
        val b = Atom("b")
        val c = Atom("c")
        val d = Atom("d")

        val runtimeEnv = runtimeWithUserClauses(
            // append([],L,L).
            app(PrologList(emptyList()), L, L),

            // append([H|T],L2,[H|L3]) :- append(T,L2,L3).)
            Rule(app(PrologList(listOf(H), T), L2, PrologList(listOf(H), L3)), PredicateInvocationQuery(app(T, L2, L3)))
        )

        "simple append" {
            runtimeEnv shouldProve app(PrologList(listOf(a, b)), PrologList(listOf(c, d)), R) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("R = [a,b,c,d]") {
                    it.variableValues[R] == PrologList(listOf(a,b,c,d))
                }
            }
        }

        "what needs to be appended?" {
            runtimeEnv shouldProve app(PrologList(listOf(a, b)), L, PrologList(listOf(a, b, c, d))) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("L = [c, d]") {
                    it.variableValues[L] == PrologList(listOf(c, d))
                }
            }
        }

        "what needs to be prepended?" {
            runtimeEnv shouldProve app(L, PrologList(listOf(c, d)), PrologList(listOf(a, b, c, d))) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("L = [a, b]") {
                    it.variableValues[L] == PrologList(listOf(a, b))
                }
            }
        }

        "what combinations are possible?" {
            val A = Variable("A")
            val B = Variable("B")

            runtimeEnv shouldProve app(A, B, PrologList(listOf(a, b))) suchThat {
                itHasExactlyNSolutions(3)

                itHasASolutionSuchThat("A = [], B = [a, b]") {
                    it.variableValues[A] == PrologList(emptyList())
                    &&
                    it.variableValues[B] == PrologList(listOf(a, b))
                }

                itHasASolutionSuchThat("A = [a], B = [b]") {
                    it.variableValues[A] == PrologList(listOf(a))
                    &&
                    it.variableValues[B] == PrologList(listOf(b))
                }

                itHasASolutionSuchThat("A = [a, b], B = []") {
                    it.variableValues[A] == PrologList(listOf(a, b))
                    &&
                    it.variableValues[B] == PrologList(emptyList())
                }
            }
        }
    }

    "retain values of random variables" {
        val X = Variable("X")
        val a = CompoundBuilder("a")
        val H = Variable("H")
        val T = Variable("T")

        val u = Atom("u")
        val v = Atom("v")

        val runtimeEnv = runtimeWithUserClauses(
            a(PrologList(listOf(H), T), PrologList(listOf(H), T))
        )

        runtimeEnv shouldProve a(X, PrologList(listOf(u, v))) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X = [u,v]") {
                val valX = it.variableValues[X] as PrologList

                valX.elements[0] == u
                &&
                valX.elements[1] == v
            }
        }
    }

    "anonymous variable" - {
        val f = CompoundBuilder("f")
        val _A = Variable.ANONYMOUS
        val X = Variable("X")
        val a = Atom("a")
        val b = Atom("b")

        "case 1" {
            val runtimeEnv = runtimeWithUserClauses(
                f(_A)
            )

            runtimeEnv shouldProve f(a) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("it is empty") {
                    it.variableValues.isEmpty
                }
            }
        }

        "case 2" {
            val runtimeEnv = runtimeWithUserClauses(
                f(_A, _A)
            )

            runtimeEnv shouldProve f(a, b) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("it is empty") {
                    it.variableValues.isEmpty
                }
            }
        }

        "case 3" {
            val runtimeEnv = runtimeWithUserClauses(
                f(_A, b)
            )

            runtimeEnv shouldProve f(a, X) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("X = b") {
                    it.variableValues[X] == b
                }
            }
        }

        "case 4" {
            val runtimeEnv = runtimeWithUserClauses(
                f(_A)
            )

            runtimeEnv shouldProve f(X) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("X = _") {
                    it.variableValues[X] is Variable
                }
            }
        }
    }

    "circular module dependency" {
        val moduleARef = ModuleReference("module", "A")
        val moduleBRef = ModuleReference("module", "B")

        val moduleA = ASTModule(
            ModuleDeclaration("A"),
            listOf(ModuleImport.Full(moduleBRef)),
            emptyList(),
            emptySet(),
            emptySet(),
            emptySet()
        )

        val moduleB = ASTModule(
            ModuleDeclaration("B"),
            listOf(ModuleImport.Full(moduleARef)),
            emptyList(),
            emptySet(),
            emptySet(),
            emptySet()
        )

        val rootModule = ASTModule(
            ModuleDeclaration("__root"),
            listOf(ModuleImport.Full(moduleARef)),
            emptyList(),
            emptySet(),
            emptySet(),
            emptySet()
        )

        val moduleLoader = PredefinedModuleLoader()
        moduleLoader.registerModule("module", moduleA)
        moduleLoader.registerModule("module", moduleB)
        moduleLoader.registerModule("module", rootModule)

        val runtimeEnv = DefaultPrologRuntimeEnvironment(moduleLoader)
        runtimeEnv.assureModulePrimed(ModuleReference("module", moduleA.declaration.moduleName))
        runtimeEnv.assureModulePrimed(ModuleReference("module", moduleB.declaration.moduleName))
        runtimeEnv.assureModulePrimed(ModuleReference("module", rootModule.declaration.moduleName))

        runtimeEnv.getLoadedModule("A") shouldBe moduleA
        runtimeEnv.getLoadedModule("B") shouldBe moduleB
    }
}}
