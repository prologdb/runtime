package com.github.prologdb.runtime

import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.matchers.shouldThrow
import io.kotlintest.specs.FreeSpec

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
            subList[0] shouldEqual 2
            subList[1] shouldEqual 3
            subList[2] shouldEqual 4
            subList[3] shouldEqual 5
            subList[4] shouldEqual 6
        }

        "should not return elements beyond length" {
            shouldThrow<ArrayIndexOutOfBoundsException> {
                subList[5]
            }
        }
    }

    "method indexOf" - {
        "should find elements in sub-range" {
            subList.indexOf(2) shouldEqual 0
            subList.indexOf(3) shouldEqual 1
            subList.indexOf(4) shouldEqual 2
            subList.indexOf(5) shouldEqual 3
            subList.indexOf(6) shouldEqual 4
        }

        "should not find elements outside of sub-range" {
            subList.indexOf(1) shouldEqual -1
            subList.indexOf(7) shouldEqual -1
            subList.indexOf(8) shouldEqual -1
        }

        "should not find elements not in the original list" {
            subList.indexOf(10) shouldEqual -1
        }
    }

    "method lastIndexOf" - {
        "should find elements in sub-range" {
            subList.indexOf(2) shouldEqual 0
            subList.indexOf(3) shouldEqual 1
            subList.indexOf(4) shouldEqual 2
            subList.indexOf(5) shouldEqual 3
            subList.indexOf(6) shouldEqual 4
        }

        "should not find elements outside of sub-range" {
            subList.indexOf(1) shouldEqual -1
            subList.indexOf(7) shouldEqual -1
            subList.indexOf(8) shouldEqual -1
        }

        "should not find elements not in the original list" {
            subList.indexOf(10) shouldEqual -1
        }

        "should return the last index" {
            val orig = listOf(1, 2, 2, 3, 4, 1, 5, 1)
            val sub = orig.subList(0, 6)

            sub.lastIndexOf(1) shouldEqual 5
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

        copy.size shouldEqual 5
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
}}