package com.github.prologdb.runtime.term

import io.kotlintest.matchers.shouldEqual
import io.kotlintest.specs.FreeSpec

class PrologStringTest : FreeSpec() { init {
    val pstr = PrologString(charArrayOf('H', 'e', 'l', 'l', 'o', ',', ' ', 'W', 'o', 'r', 'l', 'd', '!'))

    "toKotlinString" {
        pstr.toKotlinString() shouldEqual "Hello, World!"
    }

    "toString" - {
        "simple" {
            pstr.toString() shouldEqual """"Hello, World!""""
        }

        "with escape sequences" {
            val pstrWithEscapes = PrologString(charArrayOf('H', 'e', 'l', 'l', 'o', ' ', '\u000B', '\n', '\t'))

            val result = pstrWithEscapes.toString()

            result shouldEqual """"Hello \v\n\t""""
        }
    }

    "charAt" {
        pstr.charAt(0) shouldEqual 'H'
        pstr.charAt(1) shouldEqual 'e'
        pstr.charAt(2) shouldEqual 'l'
        pstr.charAt(3) shouldEqual 'l'
        pstr.charAt(4) shouldEqual 'o'
        pstr.charAt(5) shouldEqual ','

        pstr.charAt(6) shouldEqual ' '

        pstr.charAt(7) shouldEqual 'W'
        pstr.charAt(8) shouldEqual 'o'
        pstr.charAt(9) shouldEqual 'r'
        pstr.charAt(10) shouldEqual 'l'
        pstr.charAt(11) shouldEqual 'd'
        pstr.charAt(12) shouldEqual '!'
    }

    "length" {
        pstr.length shouldEqual 13
    }

    "characters" {
        val it = pstr.characters.iterator()

        it.next() shouldEqual 'H'
        it.next() shouldEqual 'e'
        it.next() shouldEqual 'l'
        it.next() shouldEqual 'l'
        it.next() shouldEqual 'o'
        it.next() shouldEqual ','
        it.next() shouldEqual ' '
        it.next() shouldEqual 'W'
        it.next() shouldEqual 'o'
        it.next() shouldEqual 'r'
        it.next() shouldEqual 'l'
        it.next() shouldEqual 'd'
        it.next() shouldEqual '!'
    }

    "substring from index" {
        pstr.substring(5) shouldEqual PrologString(", World!")
    }

    "substring from index to index" {
        pstr.substring(5, 10) shouldEqual PrologString(", Wor")
    }
}}