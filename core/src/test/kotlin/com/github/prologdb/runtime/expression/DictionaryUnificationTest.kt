package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldNotUnifyWith
import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.specs.FreeSpec

class DictionaryUnificationTest : FreeSpec() { init {
    "empty, no tag" {
        val a = PrologDictionary(null, emptyMap())
        val b = PrologDictionary(null, emptyMap())

        a shouldUnifyWith b suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("no instantiations") { it.variableValues.isEmpty }
        }
    }

    "empty, with equal tag" {
        val a = PrologDictionary(Atom("a"), emptyMap())
        val b = PrologDictionary(Atom("a"), emptyMap())

        a shouldUnifyWith b suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("no instantiations") { it.variableValues.isEmpty }
        }
    }

    "empty, with different tags" {
        val a = PrologDictionary(Atom("a"), emptyMap())
        val b = PrologDictionary(Atom("b"), emptyMap())

        a shouldNotUnifyWith b
    }

    "empty, one with variable tag, other with atom tag" {
        val B = Variable("B")
        val a = Atom("a")
        val dA = PrologDictionary(a, emptyMap())
        val dB = PrologDictionary(B, emptyMap())

        dA shouldUnifyWith dB suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("B = a") {
                it.variableValues[B] == a
            }
        }
    }

    "empty, both with variable tag" {
        val B = Variable("B")
        val A = Variable("A")
        val dA = PrologDictionary(A, emptyMap())
        val dB = PrologDictionary(B, emptyMap())

        dA shouldUnifyWith dB suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("B = A") {
                if (it.variableValues.isInstantiated(B)) {
                    it.variableValues[B] == B
                } else {
                    it.variableValues[A] == B
                }
            }
        }
    }

    "with pairs, no tags - fails because of missing values" {
        val a = Atom("a")
        val b = Atom("b")

        val dA = PrologDictionary(null, mapOf(a to a))
        val dB = PrologDictionary(null, mapOf(a to a, b to b))

        dA shouldNotUnifyWith dB
    }

    "with pairs, no tags - succeeds" {
        val a = Atom("a")
        val b = Atom("b")
        val A = Variable("A")
        val B = Variable("B")

        val dA = PrologDictionary(null, mapOf(a to a, b to B))
        val dB = PrologDictionary(null, mapOf(a to A, b to b))

        dA shouldUnifyWith dB suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("A = a, B = b") {
                it.variableValues[A] == a
                &&
                it.variableValues[B] == b
            }
        }
    }

    "with tag in pairs" {
        val a = Atom("a")
        val b = Atom("b")
        val A = Variable("A")

        val dA = PrologDictionary(a, mapOf(a to a, b to b))
        val dB = PrologDictionary(A, mapOf(a to A, b to b))

        dA shouldUnifyWith dB suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("A = a") {
                it.variableValues[A] == a
            }
        }
    }
}}