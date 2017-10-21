package com.github.tmarsteel.ktprolog.expression

import com.github.tmarsteel.ktprolog.shouldUnifyWith
import com.github.tmarsteel.ktprolog.suchThat
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Variable
import io.kotlintest.specs.FreeSpec

class AtomUnificationTest : FreeSpec() {init {
    "same object" {
        val a = Atom("a")
        a shouldUnifyWith a
    }

    "different objects, equal name" {
        Atom("a") shouldUnifyWith Atom("a")
    }

    "different objects, different names" {
        Atom("a")
    }

    "a.unify(X)" {
        val a = Atom("a")
        val X = Variable("X")

        a shouldUnifyWith X suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("instantiates X to a") {
                it.variableValues[X] == a
            }
        }
    }

    "X.unify(a)" {
        val a = Atom("a")
        val X = Variable("X")

        X shouldUnifyWith a suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("instantiates X to a") {
                it.variableValues[X] == a
            }
        }
    }
}}