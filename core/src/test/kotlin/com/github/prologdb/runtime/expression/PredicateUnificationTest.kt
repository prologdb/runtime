package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldNotUnifyWith
import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PredicateBuilder
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.specs.FreeSpec

class PredicateUnificationTest : FreeSpec(){init {
    "both equal, one argument" {
        // SETUP
        val mockAtom1 = Atom("a")
        val mockAtom2 = Atom("a")

        val fact1 = CompoundTerm("a", arrayOf(mockAtom1))
        val fact2 = CompoundTerm("a", arrayOf(mockAtom2))

        // ASSERT
        fact1 shouldUnifyWith fact2 suchThat {
            it.variableValues.isEmpty
        }
    }

    "both equal, two arguments" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomA2 = Atom("a")
        val mockAtomB1 = Atom("b")
        val mockAtomB2 = Atom("b")

        val fact1 = CompoundTerm("a", arrayOf(mockAtomA1, mockAtomB1))
        val fact2 = CompoundTerm("a", arrayOf(mockAtomA2, mockAtomB2))

        // ASSERT
        fact1 shouldUnifyWith fact2 suchThat {
            it.variableValues.isEmpty
        }
    }

    "different argument indexes" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomA2 = Atom("a")
        val mockAtomB1 = Atom("b")
        val mockAtomB2 = Atom("b")

        val fact1 = CompoundTerm("a", arrayOf(mockAtomA1, mockAtomB1))
        val fact2 = CompoundTerm("a", arrayOf(mockAtomB2, mockAtomA2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "different argument lengths" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomA2 = Atom("a")
        val mockAtomB1 = Atom("b")

        val fact1 = CompoundTerm("a", arrayOf(mockAtomA1, mockAtomB1))
        val fact2 = CompoundTerm("a", arrayOf(mockAtomA2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "non-unifying arguments" {
        // SETUP
        val mockAtomA1 = Atom("a")
        val mockAtomB1 = Atom("b")

        val fact1 = CompoundTerm("a", arrayOf(mockAtomA1))
        val fact2 = CompoundTerm("a", arrayOf(mockAtomB1))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "different functors" {
        // SETUP
        val mockAtom1 = Atom("a")
        val mockAtom2 = Atom("a")

        val fact1 = CompoundTerm("a", arrayOf(mockAtom1))
        val fact2 = CompoundTerm("b", arrayOf(mockAtom2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "different functors and non-unifying arguments" {
        // SETUP
        val mockAtom1 = Atom("a")
        val mockAtom2 = Atom("b")

        val fact1 = CompoundTerm("a", arrayOf(mockAtom1))
        val fact2 = CompoundTerm("b", arrayOf(mockAtom2))

        // ASSERT
        fact1 shouldNotUnifyWith fact2
    }

    "with variable" {
        // SETUP
        val f = PredicateBuilder("f")
        val a = Atom("a")
        val X = Variable("X")

        f(a) shouldUnifyWith f(X) suchThat {
            it.variableValues[X] == a
        }
    }

    "?- g(X, X) = g(A, B)" {
        val g = PredicateBuilder("g")
        val X = Variable("X")
        val A = Variable("A")
        val B = Variable("B")

        g(X, X) shouldUnifyWith g(A, B) suchThat {
            it.variableValues[A] == B
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
            it.variableValues[X] == s(g)
            &&
            it.variableValues[Y] == t(w)
        }
    }
}}