package com.github.prologdb.runtime

import com.github.prologdb.runtime.util.ImmutableSubList
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FreeSpec
import io.kotest.matchers.shouldBe

class ImmutableSubListTest : FreeSpec() { init {
    val list = listOf(1, 2, 3, 4, 5, 6, 7, 8)
    val subList = ImmutableSubList(list, 1, 5)

    "method contains" - {
        "should find elements in sub-range" {
            subList.contains(2) shouldBe true
            subList.contains(3) shouldBe true
            subList.contains(4) shouldBe true
            subList.contains(5) shouldBe true
            subList.contains(6) shouldBe true
        }

        "should not find elements outside of sub-range" {
            subList.contains(1) shouldBe false
            subList.contains(7) shouldBe false
            subList.contains(8) shouldBe false
        }

        "should not find elements not in the original list" {
            subList.contains(10) shouldBe false
        }
    }

    "method containsAll" - {
        "should find elements in sub-range" {
            subList.containsAll(setOf(2, 3, 4, 5, 6)) shouldBe true
        }

        "should not find elements outside of sub-range" {
            subList.containsAll(setOf(1, 7, 8)) shouldBe false
            subList.containsAll(setOf(1, 2)) shouldBe false
        }

        "should not find elements not in the original list" {
            subList.containsAll(setOf(10)) shouldBe false
        }
    }

    "method get(index)" - {
        "should return all elements in sub-list" {
            subList[0] shouldBe 2
            subList[1] shouldBe 3
            subList[2] shouldBe 4
            subList[3] shouldBe 5
            subList[4] shouldBe 6
        }

        "should not return elements beyond length" {
            shouldThrow<IndexOutOfBoundsException> {
                subList[5]
            }
        }
    }

    "method indexOf" - {
        "should find elements in sub-range" {
            subList.indexOf(2) shouldBe 0
            subList.indexOf(3) shouldBe 1
            subList.indexOf(4) shouldBe 2
            subList.indexOf(5) shouldBe 3
            subList.indexOf(6) shouldBe 4
        }

        "should not find elements outside of sub-range" {
            subList.indexOf(1) shouldBe -1
            subList.indexOf(7) shouldBe -1
            subList.indexOf(8) shouldBe -1
        }

        "should not find elements not in the original list" {
            subList.indexOf(10) shouldBe -1
        }
    }

    "method lastIndexOf" - {
        "should find elements in sub-range" {
            subList.indexOf(2) shouldBe 0
            subList.indexOf(3) shouldBe 1
            subList.indexOf(4) shouldBe 2
            subList.indexOf(5) shouldBe 3
            subList.indexOf(6) shouldBe 4
        }

        "should not find elements outside of sub-range" {
            subList.indexOf(1) shouldBe -1
            subList.indexOf(7) shouldBe -1
            subList.indexOf(8) shouldBe -1
        }

        "should not find elements not in the original list" {
            subList.indexOf(10) shouldBe -1
        }

        "should return the last index" {
            val orig = listOf(1, 2, 2, 3, 4, 1, 5, 1)
            val sub = orig.subList(0, 6)

            sub.lastIndexOf(1) shouldBe 5
        }
    }

    "method isEmpty" - {
        "is true when original list was empty" {
            val origList = emptyList<Any>()
            val sub = ImmutableSubList(origList, 0, 0)

            sub.isEmpty() shouldBe true
        }

        "is false for lists with elements" {
            subList.isEmpty() shouldBe false
        }
    }

    "iterator should yield all" {
        val copy = mutableListOf<Int>()

        subList.iterator().forEachRemaining { copy.add(it) }

        copy.size shouldBe 5
        copy.contains(2) shouldBe true
        copy.contains(3) shouldBe true
        copy.contains(4) shouldBe true
        copy.contains(5) shouldBe true
        copy.contains(6) shouldBe true
    }

    "listIterator with start index should yield appropriately" {
        val origRef = ImmutableSubList(list, 0, list.size)

        val copy = mutableListOf<Int>()
        origRef.listIterator(3).forEachRemaining { copy.add(it) }
        copy.size shouldBe 5
        copy.contains(4) shouldBe true
        copy.contains(5) shouldBe true
        copy.contains(6) shouldBe true
        copy.contains(7) shouldBe true
        copy.contains(8) shouldBe true
    }

    "equality with default lits impls" {
        val stdLibList = listOf(2, 3, 4, 5)
        val immutableSubList = ImmutableSubList(listOf(1, 2, 3, 4, 5, 6), 1, 4)

        stdLibList shouldBe immutableSubList
        immutableSubList shouldBe stdLibList
    }
}}