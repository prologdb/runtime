package com.github.prologdb.runtime

import com.github.prologdb.runtime.util.ArityMap
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.ints.beGreaterThanOrEqualTo
import io.kotest.matchers.should
import io.kotest.matchers.shouldBe

class ArityMapTest : FreeSpec() {init {
    "arities set through set() should be containes" {
        val map = ArityMap<String>()
        map[0] = "foobar"
        map[7] = "barfoo"

        map.contains(0) shouldBe true
        map.contains(7) shouldBe true
    }

    "values set through set() should be available through get()" {
        val map = ArityMap<String>()
        map[0] = "foobar"
        map[2] = "barfoo"

        map[0] shouldBe "foobar"
        map[2] shouldBe "barfoo"
    }

    "capacity should increase as necessary" {
        val map = ArityMap<String>()
        map[0] = "foobar"
        map.capacity should beGreaterThanOrEqualTo(0)

        map[14] = "barfoo"
        map.capacity should beGreaterThanOrEqualTo(14)
    }

    "values removed should not be contained anymore" {
        val map = ArityMap<String>()

        map[4] = "foobar"

        map.contains(4) shouldBe true

        map.remove(4)

        map.contains(4) shouldBe false
        map[4] shouldBe null
    }
}}