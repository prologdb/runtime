package com.github.tmarsteel.ktprolog.expression

import com.github.tmarsteel.ktprolog.shouldNotUnifyWith
import com.github.tmarsteel.ktprolog.shouldUnifyWith
import com.github.tmarsteel.ktprolog.suchThat
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.PredicateBuilder
import com.github.tmarsteel.ktprolog.term.Variable
import io.kotlintest.specs.FreeSpec

class PredicateUnificationTest : FreeSpec(){init {
    "both equal, one argument" {
        // SETUP
        val mockAtom1 = Atom("a")
        val mockAtom2 = Atom("a")

        val fact1 = Predicate("a", arrayOf(mockAtom1))
        val fact2 = Predicate("a", arrayOf(mockAtom2))

        // ASSERT
        fact1 shouldUnifyWith fact2 suchThat { itHasExactlyOneSolution() }
    }

    "both equal, two arguments" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomA2 = Atom("a")
        val mockAtomB1 = Atom("b")
        val mockAtomB2 = Atom("b")

        val fact1 = Predicate("a", arrayOf(mockAtomA1, mockAtomB1))
        val fact2 = Predicate("a", arrayOf(mockAtomA2, mockAtomB2))

        // ASSERT
        fact1 shouldUnifyWith fact2 suchThat { itHasExactlyOneSolution() }
    }

    "different argument indexes" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomA2 = Atom("a")
        val mockAtomB1 = Atom("b")
        val mockAtomB2 = Atom("b")

        val fact1 = Predicate("a", arrayOf(mockAtomA1, mockAtomB1))
        val fact2 = Predicate("a", arrayOf(mockAtomB2, mockAtomA2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "different argument lengths" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomA2 = Atom("a")
        val mockAtomB1 = Atom("b")

        val fact1 = Predicate("a", arrayOf(mockAtomA1, mockAtomB1))
        val fact2 = Predicate("a", arrayOf(mockAtomA2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "non-unifying arguments" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomB1 = Atom("b")

        val fact1 = Predicate("a", arrayOf(mockAtomA1))
        val fact2 = Predicate("a", arrayOf(mockAtomB1))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "different predicate names" {
        // SETUP
        val mockAtom1 = Atom("a")
        val mockAtom2 = Atom("a")

        val fact1 = Predicate("a", arrayOf(mockAtom1))
        val fact2 = Predicate("b", arrayOf(mockAtom2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "different predicate names and non-unifying arguments" {
        // SETUP
        val mockAtom1 = Atom("a")
        val mockAtom2 = Atom("b")

        val fact1 = Predicate("a", arrayOf(mockAtom1))
        val fact2 = Predicate("b", arrayOf(mockAtom2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "with variable" {
        // SETUP
        val f = PredicateBuilder("f")
        val a = Atom("a")
        val X = Variable("X")

        f(a) shouldUnifyWith f(X) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X is instantiated to a") {
                it.variableValues[X] == a
            }
        }
    }

    "?- g(X, X) = g(A, B)" {
        val g = PredicateBuilder("g")
        val X = Variable("X")
        val A = Variable("A")
        val B = Variable("B")

        g(X, X) shouldUnifyWith g(A, B) suchThat {
            itHasASolutionSuchThat("A = B") {
                it.variableValues[A] == B
            }
        }
    }

    "?- f(a, X) = f(X, b)" {
        // SETUP
        val a = Atom("a")
        val b = Atom("b")
        val X = Variable("X")
        val f = PredicateBuilder("f")

        // ASSERT
        f(a, X) shouldNotUnifyWith f(X, b)
    }

    "?- k(s(g),Y) = k(X,t(w))" {
        // SETUP
        val g = Atom("g")
        val w = Atom("w")
        val Y = Variable("Y")
        val X = Variable("X")
        val s = PredicateBuilder("s")
        val k = PredicateBuilder("k")
        val t = PredicateBuilder("t")

        val lhs = k(s((g)), Y)
        val rhs = k(X, t(w))

        lhs shouldUnifyWith rhs suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X = s(g), Y = t(w)") {
                it.variableValues[X] == s(g)
                &&
                it.variableValues[Y] == t(w)
            }
        }
    }
}}