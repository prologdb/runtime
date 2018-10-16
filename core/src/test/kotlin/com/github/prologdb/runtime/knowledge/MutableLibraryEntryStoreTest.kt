package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.remainingTo
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.forOne
import io.kotlintest.matchers.*
import io.kotlintest.mock.mock
import io.kotlintest.specs.FreeSpec

class MutableLibraryEntryStoreTest : FreeSpec() {
    override val oneInstancePerTest = true
init {
    val implementationsToTest: Array<() -> MutableLibraryEntryStore> = arrayOf(
        { SimpleLibraryEntryStore() },
        { DoublyIndexedLibraryEntryStore() }
    )

    for (implementationFactory in implementationsToTest) {
        var entryStore = implementationFactory()
        "${entryStore::class.simpleName}" - {
            "facts added through add() should be returned through findFor()" {
                // SETUP
                entryStore = implementationFactory()
                val predicate = Predicate("foo", arrayOf(Atom("x"), Atom("y")))
                entryStore.add(predicate)

                // ACT
                val result = entryStore.findFor(predicate).toList()

                // ASSERT
                result.size shouldEqual 1
                result.first() shouldEqual predicate
            }

            "facts added through include() should be returned through findFor()" {
                // SETUP
                entryStore = implementationFactory()
                val prediate = Predicate("pred", arrayOf(Atom("y")))
                val fakeLibrary = object : LibraryEntryStore {
                    override val exports: Iterable<LibraryEntry> = listOf(prediate)
                }

                // ACT
                entryStore.include(fakeLibrary)
                val result = entryStore.findFor(prediate).toList()

                // ASSERT
                result.size shouldEqual 1
                result.first() shouldEqual prediate
            }

            "findFor should respect arity" {
                // SETUP
                entryStore = implementationFactory()
                val entryArity1 = Predicate("foo", arrayOf(Atom("x")))
                val entryArity2 = Rule(Predicate("foo", arrayOf(Atom("x"), Atom("y"))), PredicateQuery(Predicate("hans", arrayOf())))

                // ACT
                entryStore.add(entryArity1)
                entryStore.add(entryArity2)
                val resultArity1 = entryStore.findFor(entryArity1).toList()
                val resultArity2 = entryStore.findFor(entryArity2.head).toList()

                // ASSERT
                resultArity1.size shouldEqual 1
                resultArity1.first().arity shouldEqual 1

                resultArity2.size shouldEqual  1
                resultArity2.first().arity shouldEqual 2
            }

            "findFor should respect functor" {
                // SETUP
                entryStore = implementationFactory()
                val entryFunctorA = Predicate("foobar", arrayOf(Atom("x")))
                val entryFunctorB = Rule(Predicate("barfoo", arrayOf(Atom("x"))), PredicateQuery(Predicate("hans", arrayOf())))

                // ACT
                entryStore.add(entryFunctorA)
                entryStore.add(entryFunctorB)
                val resultFunctorA = entryStore.findFor(entryFunctorA).toList()
                val resultFunctorB = entryStore.findFor(entryFunctorB.head).toList()

                // ASSERT
                resultFunctorA.size shouldEqual 1
                resultFunctorA.first().name shouldEqual entryFunctorA.name

                resultFunctorB.size shouldEqual 1
                resultFunctorB.first().name shouldEqual entryFunctorB.name
            }

            "retraction" - {
                // SETUP
                entryStore = implementationFactory()
                val factA = Predicate("foobar", arrayOf(Atom("x")))
                val factB = Predicate("barfoo", arrayOf(Atom("x")))

                // ACT
                entryStore.add(factA)
                entryStore.add(factB)
                entryStore.exports.toList() should contain<LibraryEntry>(factA)
                entryStore.exports.toList() should contain<LibraryEntry>(factA)

                "facts removed through retractFact" - {

                    entryStore.retractFact(factA).tryAdvance()

                    "should not be contained in exports" {
                        // ASSERT
                        entryStore.exports.toList() shouldNot contain<LibraryEntry>(factA)
                        entryStore.exports.toList() should contain<LibraryEntry>(factB)
                    }

                    "should not be returned by findFor" {
                        // ASSERT
                        entryStore.findFor(factA).toList() should beEmpty()
                        entryStore.findFor(factB).toList() shouldNot beEmpty()
                    }
                }

                "facts removed through retract" - {
                    entryStore.retract(factA)

                    "should not be contained in exports" {
                        // ASSERT
                        entryStore.exports.toList() shouldNot contain<LibraryEntry>(factA)
                        entryStore.exports.toList() should contain<LibraryEntry>(factB)
                    }

                    "should not be returned by findFor" {
                        // ASSERT
                        entryStore.findFor(factA).toList() should beEmpty()
                        entryStore.findFor(factB).toList() shouldNot beEmpty()
                    }
                }

                "rules removed through retract" - {
                    val rule = Rule(Predicate("head", arrayOf(Variable("X"))), PredicateQuery(factA))
                    entryStore.add(rule)
                    entryStore.findFor(rule.head).toList().size shouldEqual 1

                    entryStore.retract(rule.head).tryAdvance()

                    "should not be contained in exports" {
                        entryStore.exports.toList() shouldNot contain<LibraryEntry>(rule)
                    }

                    "should not be returned by findFor" {
                        entryStore.findFor(rule.head).toList() should beEmpty()
                    }
                }

                "retractFact should retract only first fact" {
                    // SETUP
                    val fact1 = Predicate("foo", arrayOf(Atom("x")))
                    val fact2 = Predicate("foo", arrayOf(Atom("x")))

                    entryStore.add(fact1)
                    entryStore.add(fact2)

                    entryStore.findFor(fact1).toList().size shouldEqual 2

                    // ACT
                    entryStore.retractFact(fact2).tryAdvance()
                    val result = entryStore.findFor(fact2).toList()

                    // ASSERT
                    result.size shouldEqual 1
                }

                "retract should retract first fact" {
                    // SETUP
                    val fact1 = Predicate("foo", arrayOf(Atom("x")))
                    val fact2 = Predicate("foo", arrayOf(Atom("x")))

                    entryStore.add(fact1)
                    entryStore.add(fact2)

                    entryStore.findFor(fact1).toList().size shouldEqual 2

                    // ACT
                    entryStore.retract(fact2).tryAdvance()
                    val result = entryStore.findFor(fact2).toList()

                    // ASSERT
                    result.size shouldEqual 1
                }

                "retractAllFacts should retract all facts" {
                    // SETUP
                    entryStore = implementationFactory()
                    val fact1 = Predicate("foo", arrayOf(Atom("x")))
                    val fact2 = Predicate("foo", arrayOf(Atom("x")))

                    entryStore.add(fact1)
                    entryStore.add(fact2)

                    entryStore.findFor(fact1).toList().size shouldEqual 2

                    // ACT
                    entryStore.retractAllFacts(fact1)

                    // ASSERT
                    entryStore.exports.toList()should beEmpty()
                    entryStore.findFor(fact1).toList() should beEmpty()
                    entryStore.findFor(fact2).toList() should beEmpty()
                }

                "retractAll should retract all facts and rules" {
                    // SETUP
                    entryStore = implementationFactory()
                    val fact1 = Predicate("foo", arrayOf(Atom("x")))
                    val fact2 = Predicate("foo", arrayOf(Atom("x")))
                    val rule2 = Rule(fact1, mock<Query>())

                    entryStore.add(fact1)
                    entryStore.add(fact2)
                    entryStore.add(rule2)

                    entryStore.findFor(fact1).toList().size shouldEqual 3

                    // ACT
                    entryStore.retractAll(fact1)

                    // ASSERT
                    entryStore.exports.toList() should beEmpty()
                    entryStore.findFor(fact2).toList() should beEmpty()
                }

                "retract should yield unifications" - {
                    "retractFact" {
                        // SETUP
                        entryStore = implementationFactory()
                        val fact = Predicate("uutqwe", arrayOf(Atom("x")))
                        val retractionFact = Predicate("uutqwe", arrayOf(Variable("X")))
                        val expectedUnification = fact.unify(retractionFact)!!

                        // ACT
                        entryStore.add(fact)
                        val result = entryStore.retractFact(retractionFact)

                        // ASSERT
                        result.remainingTo(::ArrayList) shouldEqual listOf(expectedUnification)
                    }

                    "retract" {
                        // SETUP
                        entryStore = implementationFactory()

                        val fact           = Predicate("foo", arrayOf(Atom("y")))
                        val ruleHead       = Predicate("foo", arrayOf(Atom("aazwe")))
                        val retractionFact = Predicate("foo", arrayOf(Variable("Z")))
                        val rule = Rule(ruleHead, mock<Query>())

                        val expectedFactUnification = fact.unify(retractionFact)
                        val expectedRuleUnification = ruleHead.unify(retractionFact)

                        entryStore.add(fact)
                        entryStore.add(rule)

                        // ACT
                        val result = entryStore.retract(retractionFact).remainingTo(::ArrayList)

                        // ASSERT
                        result.size shouldEqual 2
                        forOne(result) {
                            it shouldEqual expectedFactUnification
                        }
                        forOne(result) {
                            it shouldEqual expectedRuleUnification
                        }
                    }
                }
            }
        }
    }
}}