package com.github.tmarsteel.ktprolog.expression

import com.github.tmarsteel.ktprolog.shouldNotProve
import com.github.tmarsteel.ktprolog.shouldProve
import com.github.tmarsteel.ktprolog.suchThat
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Variable
import com.github.tmarsteel.ktprolog.term.List
import com.github.tmarsteel.ktprolog.knowledge.DefaultKnowledgeBase
import com.github.tmarsteel.ktprolog.knowledge.RandomVariable
import com.github.tmarsteel.ktprolog.knowledge.Rule
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.PredicateBuilder
import io.kotlintest.specs.FreeSpec

class KnowledgeBaseTest : FreeSpec() {init {
    "f(a). f(b). ?- f(X)" {
        val kb = DefaultKnowledgeBase()
        val a = Atom("a")
        val b = Atom("b")
        kb.assert(Predicate("f", arrayOf(a)))
        kb.assert(Predicate("f", arrayOf(b)))

        val X = Variable("X")
        val queryFact = Predicate("f", arrayOf(X))

        kb shouldProve queryFact suchThat {
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
        val kb = DefaultKnowledgeBase()
        val f = PredicateBuilder("f")
        val a = PredicateBuilder("a")
        val m = Atom("m")
        val n = Atom("n")
        val X = Variable("X")
        val Y = Variable("Y")

        kb.assert(f(a(X, Y), a(Y, X)))
        kb shouldProve f(a(m, n), X) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X = a(n, m)") {
                it.variableValues[X] == a(n, m)
            }
        }
    }

    "temporary variables" {
        val kb = DefaultKnowledgeBase()
        val f = PredicateBuilder("f")
        val a = Atom("a")
        val b = Atom("b")
        val c = Atom("c")
        val X = Variable("X")
        val Y = Variable("Y")
        val Z = Variable("Z")
        kb.assert(f(a, b))
        kb.assert(f(c))

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
        val vertical = PredicateBuilder("vertical")
        val horizontal = PredicateBuilder("horizontal")
        val line = PredicateBuilder("line")
        val point = PredicateBuilder("point")
        val a = Atom("a")
        val b = Atom("b")
        val c = Atom("c")
        val X = Variable("X")
        val Y = Variable("Y")
        val Z = Variable("Z")
        val P = Variable("P")

        val kb = DefaultKnowledgeBase()
        kb.assert(
            vertical(line(point(X,Y),point(X,Z)))
        )
        kb.assert(
            horizontal(line(point(X,Y),point(Z,Y)))
        )

        // ASSERT
        kb shouldProve vertical(line(point(a,a),point(a,c))) suchThat {
            itHasExactlyOneSolution()
        }

        kb shouldNotProve vertical(line(point(a,a),point(c,b)))

        kb shouldProve horizontal(line(point(a,a),point(b,Y))) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("Y = a") {
                it.variableValues[Y] == a
            }
        }

        kb shouldProve horizontal(line(point(b,c),P)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("P = point(_R,c)") {
                val valP = it.variableValues[P]

                valP is Predicate && valP.arguments.size == 2 && valP.arguments[0] is RandomVariable && valP.arguments[1] == c
            }
        }
    }

    "g(X, X). f(X, Y) :- g(X, Y). ?- f(a, V)" {
        val kb = DefaultKnowledgeBase()

        val f = PredicateBuilder("f")
        val g = PredicateBuilder("g")
        val X = Variable("X")
        val Y = Variable("Y")
        val a = Atom("a")
        val V = Variable("V")

        kb.defineRule(Rule(f(X, Y), listOf(g(X, Y))))
        kb.assert(g(X, X))

        kb shouldProve f(a, V) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("V = a") {
                it.variableValues[V] == a
            }
        }
    }

    "list append" {
        val kb = DefaultKnowledgeBase()

        val append = PredicateBuilder("append")
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


        // append([],L,L).
        kb.assert(append(List(emptyList()),L,L))

        // append([H|T],L2,[H|L3]) :- append(T,L2,L3).)
        kb.defineRule(Rule(append(List(listOf(H),T),L2,List(listOf(H),L3)), listOf(append(T,L2,L3))))

        kb shouldProve append(List(listOf(a, b)),List(listOf(c,d)),R) suchThat {
            // itHasExactlyOneSolution()
            itHasASolutionSuchThat("R = [a,b,c,d]") {
                it.variableValues[R] == List(listOf(a,b,c,d))
            }
        }
    }

    "anonymous variable" - {
        val kb = DefaultKnowledgeBase()
        val f = PredicateBuilder("f")
        val _A = Variable.ANONYMOUS
        val X = Variable("X")
        val a = Atom("a")
        val b = Atom("b")

        "case 1" {
            kb.assert(f(_A))

            kb shouldProve f(a) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("it is empty") {
                    it.variableValues.isEmpty
                }
            }
        }

        "case 2" {
            kb.assert(f(_A, _A))

            kb shouldProve f(a, b) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("it is empty") {
                    it.variableValues.isEmpty
                }
            }
        }

        "case 3" {
            kb.assert(f(_A, b))

            kb shouldProve f(a, X) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("X = b") {
                    it.variableValues[X] == b
                }
            }
        }

        "case 4" {
            kb.assert(f(_A))

            kb shouldProve f(X) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("X = _") {
                    it.variableValues[X] == _A
                }
            }
        }
    }
}}
