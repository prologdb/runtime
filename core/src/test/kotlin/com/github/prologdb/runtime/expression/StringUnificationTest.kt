package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldNotUnifyWith
import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable
import io.kotest.core.spec.style.FreeSpec

class StringUnificationTest : FreeSpec() { init {
    "equality" {
        PrologString("Foobar") shouldUnifyWith PrologString("Foobar")
    }

    "unequality" - {
        "by characters" {
            PrologString("foobar") shouldNotUnifyWith PrologString("barfoo")
        }

        "by case-sensitivity" {
            PrologString("Foobar") shouldNotUnifyWith PrologString("foobar")
        }
    }

    "head and tail separation" - {
        "head - one char" {
            val H = Variable("H")
            val T = Variable("T")
            PrologString("foobar") shouldUnifyWith PrologList(listOf(H), T) suchThat {
                it.variableValues[H] == PrologNumber(102)
                &&
                it.variableValues[T] == PrologList(listOf(
                    PrologNumber(111),
                    PrologNumber(111),
                    PrologNumber(98),
                    PrologNumber(97),
                    PrologNumber(114)
                ))
            }
        }

        "head - three chars" {
            val H1 = Variable("H1")
            val H2 = Variable("H2")
            val H3 = Variable("H3")
            val T = Variable("T")
            PrologString("foobar") shouldUnifyWith PrologList(listOf(H1, H2, H3), T) suchThat {
                it.variableValues[H1] == PrologNumber(102)
                &&
                it.variableValues[H2] == PrologNumber(111)
                &&
                it.variableValues[H3] == PrologNumber(111)
                &&
                it.variableValues[T] == PrologList(listOf(
                    PrologNumber(98),
                    PrologNumber(97),
                    PrologNumber(114)
                ))
            }
        }
    }
}}
