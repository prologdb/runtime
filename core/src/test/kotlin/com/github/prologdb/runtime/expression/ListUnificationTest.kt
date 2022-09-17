package com.github.prologdb.runtime.expression

import com.github.prologdb.runtime.shouldNotUnifyWith
import com.github.prologdb.runtime.shouldUnifyWith
import com.github.prologdb.runtime.suchThat
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Variable
import io.kotest.core.spec.style.FreeSpec

class ListUnificationTest : FreeSpec() {init {
    val a = Atom("a")
    val b = Atom("b")
    val X = Variable("X")
    val H = Variable("H")
    val T = Variable("T")

    "[a] = [a]" {
        PrologList(listOf(a)) shouldUnifyWith PrologList(listOf(a))
    }

    "[a] \\== a" {
        PrologList(listOf(a)) shouldNotUnifyWith a
    }

    "a \\== [a]" {
        a shouldNotUnifyWith PrologList(listOf(a))
    }

    "[a] = [X]" {
        PrologList(listOf(a)) shouldUnifyWith PrologList(listOf(X)) suchThat {
            it[X] == a
        }
    }

    "[H|T] = [a]" {
        PrologList(listOf(H), T) shouldUnifyWith PrologList(listOf(a)) suchThat {
            it[H] == a
            &&
            it[T] == PrologList(emptyList())
        }
    }

    "[a] = [H|T]" {
        PrologList(listOf(a)) shouldUnifyWith PrologList(listOf(H), T) suchThat {
            it[H] == a
            &&
            it[T] == PrologList(emptyList())
        }
    }

    "[H|T] = [a,b]" {
        PrologList(listOf(H), T) shouldUnifyWith PrologList(listOf(a,b)) suchThat {
            it[H] == a
            &&
            it[T] == PrologList(listOf(b))
        }
    }

    "[a] \\== [b]" {
        PrologList(listOf(a)) shouldNotUnifyWith PrologList(listOf(b))
    }

    "[a|T] \\== [b,b]" {
        PrologList(listOf(a), T) shouldNotUnifyWith PrologList(listOf(b, b))
    }

    "[H,H] \\== [a,b]" {
        PrologList(listOf(H, H)) shouldNotUnifyWith PrologList(listOf(a, b))
    }

    "[H|H] \\== [[a],b]" {
        PrologList(listOf(H), H) shouldNotUnifyWith PrologList(listOf(PrologList(listOf(a)), b))
    }

    "[T1,T2,x|T1] \\== [[a],[b],x|T2]." {
        val T1 = Variable("T1")
        val T2 = Variable("T2")
        val x = Atom("x")

        PrologList(listOf(T1, T2, x), T1) shouldNotUnifyWith PrologList(listOf(PrologList(listOf(a)), PrologList(listOf(b)), x), T2)
    }
}}