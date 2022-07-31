package com.github.prologdb.runtime.term

import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class PrologStringTest : FreeSpec() { init {
    val pstr = PrologString(charArrayOf('H', 'e', 'l', 'l', 'o', ',', ' ', 'W', 'o', 'r', 'l', 'd', '!'))

    "toKotlinString" {
        pstr.toKotlinString() shouldBe "Hello, World!"
    }

    "toString" - {
        "simple" {
            pstr.toString() shouldBe """"Hello, World!""""
        }

        "with escape sequences" {
            val pstrWithEscapes = PrologString(charArrayOf('H', 'e', 'l', 'l', 'o', ' ', '\u000B', '\n', '\t'))

            val result = pstrWithEscapes.toString()

            result shouldBe """"Hello \v\n\t""""
        }
    }

    "charAt" {
        pstr.charAt(0) shouldBe 'H'
        pstr.charAt(1) shouldBe 'e'
        pstr.charAt(2) shouldBe 'l'
        pstr.charAt(3) shouldBe 'l'
        pstr.charAt(4) shouldBe 'o'
        pstr.charAt(5) shouldBe ','

        pstr.charAt(6) shouldBe ' '

        pstr.charAt(7) shouldBe 'W'
        pstr.charAt(8) shouldBe 'o'
        pstr.charAt(9) shouldBe 'r'
        pstr.charAt(10) shouldBe 'l'
        pstr.charAt(11) shouldBe 'd'
        pstr.charAt(12) shouldBe '!'
    }

    "length" {
        pstr.length shouldBe 13
    }

    "characters" {
        val it = pstr.characters.iterator()

        it.next() shouldBe 'H'
        it.next() shouldBe 'e'
        it.next() shouldBe 'l'
        it.next() shouldBe 'l'
        it.next() shouldBe 'o'
        it.next() shouldBe ','
        it.next() shouldBe ' '
        it.next() shouldBe 'W'
        it.next() shouldBe 'o'
        it.next() shouldBe 'r'
        it.next() shouldBe 'l'
        it.next() shouldBe 'd'
        it.next() shouldBe '!'
    }

    "substring from index" {
        pstr.substring(5) shouldBe PrologString(", World!")
    }

    "substring from index to index" {
        pstr.substring(5, 10) shouldBe PrologString(", Wor")
    }
}}