package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.Variable
import io.kotest.core.spec.style.FreeSpec

class AtomUnificationTest : FreeSpec() {init {
    "same object" {
        val a = Atom("a")
        a shouldUnifyWith a
    }

    "different objects, equal functor" {
        Atom("a") shouldUnifyWith Atom("a")
    }

    "different objects, different names" {
        Atom("a")
    }

    "a.unify(X)" {
        val a = Atom("a")
        val X = Variable("X")

        a shouldUnifyWith X suchThat {
             it.variableValues[X] == a
        }
    }

    "X.unify(a)" {
        val a = Atom("a")
        val X = Variable("X")

        X shouldUnifyWith a suchThat {
            it.variableValues[X] == a
        }
    }
}}