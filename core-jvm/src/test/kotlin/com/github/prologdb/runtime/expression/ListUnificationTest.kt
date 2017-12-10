package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldNotUnifyWith
import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.List
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.specs.FreeSpec

class ListUnificationTest : FreeSpec() {init {
    val a = Atom("a")
    val b = Atom("b")
    val X = Variable("X")
    val H = Variable("H")
    val T = Variable("T")

    "[a] = [a]" {
        List(listOf(a)) shouldUnifyWith List(listOf(a))
    }

    "[a] \\== a" {
        List(listOf(a)) shouldNotUnifyWith a
    }

    "a \\== [a]" {
        a shouldNotUnifyWith List(listOf(a))
    }

    "[a] = [X]" {
        List(listOf(a)) shouldUnifyWith List(listOf(X)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("X = a") {
                it.variableValues[X] == a
            }
        }
    }

    "[H|T] = [a]" {
        List(listOf(H), T) shouldUnifyWith List(listOf(a)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("H = a, T = []") {
                it.variableValues[H] == a
                &&
                it.variableValues[T] == List(emptyList())
            }
        }
    }

    "[a] = [H|T]" {
        List(listOf(a)) shouldUnifyWith List(listOf(H), T) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("H = a, T = []") {
                it.variableValues[H] == a
                &&
                it.variableValues[T] == List(emptyList())
            }
        }
    }

    "[H|T] = [a,b]" {
        List(listOf(H), T) shouldUnifyWith List(listOf(a,b)) suchThat {
            itHasExactlyOneSolution()
            itHasASolutionSuchThat("H = a, T = [b]") {
                it.variableValues[H] == a
                &&
                it.variableValues[T] == List(listOf(b))
            }
        }
    }

    "[a] \\== [b]" {
        List(listOf(a)) shouldNotUnifyWith List(listOf(b))
    }

    "[a|T] \\== [b,b]" {
        List(listOf(a), T) shouldNotUnifyWith List(listOf(b, b))
    }

    "[H,H] \\== [a,b]" {
        List(listOf(H, H)) shouldNotUnifyWith List(listOf(a, b))
    }

    "[H|H] \\== [[a],b]" {
        List(listOf(H), H) shouldNotUnifyWith List(listOf(List(listOf(a)), b))
    }

    "[T1,T2,x|T1] \\== [[a],[b],x|T2]." {
        val T1 = Variable("T1")
        val T2 = Variable("T2")
        val x = Atom("x")

        List(listOf(T1, T2, x), T1) shouldNotUnifyWith List(listOf(List(listOf(a)), List(listOf(b)), x), T2)
    }
}}