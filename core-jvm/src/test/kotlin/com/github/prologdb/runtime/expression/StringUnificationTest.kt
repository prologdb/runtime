package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldNotUnifyWith
import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.specs.FreeSpec
import com.github.prologdb.runtime.term.Integer as PrologInteger

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
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("H = 102, T = [111,111,98,97,114]") {
                    it.variableValues[H] == PrologInteger(102)
                    &&
                    it.variableValues[T] == PrologList(listOf(
                        PrologInteger(111),
                        PrologInteger(111),
                        PrologInteger(98),
                        PrologInteger(97),
                        PrologInteger(114)
                    ))
                }
            }
        }

        "head - three chars" {
            val H1 = Variable("H1")
            val H2 = Variable("H2")
            val H3 = Variable("H3")
            val T = Variable("T")
            PrologString("foobar") shouldUnifyWith PrologList(listOf(H1, H2, H3), T) suchThat {
                itHasExactlyOneSolution()
                itHasASolutionSuchThat("H1 = 102, H2 = 111, H3 = 111, T = [98,97,114]") {
                    it.variableValues[H1] == PrologInteger(102)
                    &&
                    it.variableValues[H2] == PrologInteger(111)
                    &&
                    it.variableValues[H3] == PrologInteger(111)
                    &&
                    it.variableValues[T] == PrologList(listOf(

                        PrologInteger(98),
                        PrologInteger(97),
                        PrologInteger(114)
                    ))
                }
            }
        }
    }
}}
