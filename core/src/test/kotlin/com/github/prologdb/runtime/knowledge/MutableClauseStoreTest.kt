package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.remainingTo
import com.github.prologdb.runtime.knowledge.library.Clause
import com.github.prologdb.runtime.knowledge.library.DoublyIndexedClauseStore
import com.github.prologdb.runtime.knowledge.library.MutableClauseStore
import com.github.prologdb.runtime.knowledge.library.SimpleClauseStore
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import io.kotlintest.forOne
import io.kotlintest.matchers.*
import io.kotlintest.mock.mock
import io.kotlintest.specs.FreeSpec

class MutableClauseStoreTest : FreeSpec() {
    override val oneInstancePerTest = true
init {
    val implementationsToTest: Array<() -> MutableClauseStore> = arrayOf(
        { SimpleClauseStore() },
        { DoublyIndexedClauseStore() }
    )

    for (implementationFactory in implementationsToTest) {
        var entryStore = implementationFactory()
        "${entryStore::class.simpleName}" - {
            "facts added through add() should be returned through findFor()" {
                // SETUP
                entryStore = implementationFactory()
                val compound = CompoundTerm("foo", arrayOf(Atom("x"), Atom("y")))
                entryStore.assertz(compound)

                // ACT
                val result = entryStore.findFor(compound).toList()

                // ASSERT
                result.size shouldEqual 1
                result.first() shouldEqual compound
            }

            "findFor should respect arity" {
                // SETUP
                entryStore = implementationFactory()
                val entryArity1 = CompoundTerm("foo", arrayOf(Atom("x")))
                val entryArity2 = Rule(CompoundTerm("foo", arrayOf(Atom("x"), Atom("y"))), PredicateInvocationQuery(CompoundTerm("hans", arrayOf())))

                // ACT
                entryStore.assertz(entryArity1)
                entryStore.assertz(entryArity2)
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
                val entryFunctorA = CompoundTerm("foobar", arrayOf(Atom("x")))
                val entryFunctorB = Rule(CompoundTerm("barfoo", arrayOf(Atom("x"))), PredicateInvocationQuery(CompoundTerm("hans", arrayOf())))

                // ACT
                entryStore.assertz(entryFunctorA)
                entryStore.assertz(entryFunctorB)
                val resultFunctorA = entryStore.findFor(entryFunctorA).toList()
                val resultFunctorB = entryStore.findFor(entryFunctorB.head).toList()

                // ASSERT
                resultFunctorA.size shouldEqual 1
                resultFunctorA.first().functor shouldEqual entryFunctorA.functor

                resultFunctorB.size shouldEqual 1
                resultFunctorB.first().functor shouldEqual entryFunctorB.functor
            }

            "retraction" - {
                // SETUP
                entryStore = implementationFactory()
                val factA = CompoundTerm("foobar", arrayOf(Atom("x")))
                val factB = CompoundTerm("barfoo", arrayOf(Atom("x")))

                // ACT
                entryStore.assertz(factA)
                entryStore.assertz(factB)
                entryStore.clauses.toList() should contain<Clause>(factA)
                entryStore.clauses.toList() should contain<Clause>(factA)

                "facts removed through retractFact" - {

                    entryStore.retractFact(factA).tryAdvance()

                    "should not be contained in exports" {
                        // ASSERT
                        entryStore.clauses.toList() shouldNot contain<Clause>(factA)
                        entryStore.clauses.toList() should contain<Clause>(factB)
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
                        entryStore.clauses.toList() shouldNot contain<Clause>(factA)
                        entryStore.clauses.toList() should contain<Clause>(factB)
                    }

                    "should not be returned by findFor" {
                        // ASSERT
                        entryStore.findFor(factA).toList() should beEmpty()
                        entryStore.findFor(factB).toList() shouldNot beEmpty()
                    }
                }

                "rules removed through retract" - {
                    val rule = Rule(CompoundTerm("head", arrayOf(Variable("X"))), PredicateInvocationQuery(factA))
                    entryStore.assertz(rule)
                    entryStore.findFor(rule.head).toList().size shouldEqual 1

                    entryStore.retract(rule.head).tryAdvance()

                    "should not be contained in exports" {
                        entryStore.clauses.toList() shouldNot contain<Clause>(rule)
                    }

                    "should not be returned by findFor" {
                        entryStore.findFor(rule.head).toList() should beEmpty()
                    }
                }

                "retractFact should retract only first fact" {
                    // SETUP
                    val fact1 = CompoundTerm("foo", arrayOf(Atom("x")))
                    val fact2 = CompoundTerm("foo", arrayOf(Atom("x")))

                    entryStore.assertz(fact1)
                    entryStore.assertz(fact2)

                    entryStore.findFor(fact1).toList().size shouldEqual 2

                    // ACT
                    entryStore.retractFact(fact2).tryAdvance()
                    val result = entryStore.findFor(fact2).toList()

                    // ASSERT
                    result.size shouldEqual 1
                }

                "retract should retract first fact" {
                    // SETUP
                    val fact1 = CompoundTerm("foo", arrayOf(Atom("x")))
                    val fact2 = CompoundTerm("foo", arrayOf(Atom("x")))

                    entryStore.assertz(fact1)
                    entryStore.assertz(fact2)

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
                    val fact1 = CompoundTerm("foo", arrayOf(Atom("x")))
                    val fact2 = CompoundTerm("foo", arrayOf(Atom("x")))

                    entryStore.assertz(fact1)
                    entryStore.assertz(fact2)

                    entryStore.findFor(fact1).toList().size shouldEqual 2

                    // ACT
                    entryStore.retractAllFacts(fact1)

                    // ASSERT
                    entryStore.clauses.toList()should beEmpty()
                    entryStore.findFor(fact1).toList() should beEmpty()
                    entryStore.findFor(fact2).toList() should beEmpty()
                }

                "retractAll should retract all facts and rules" {
                    // SETUP
                    entryStore = implementationFactory()
                    val fact1 = CompoundTerm("foo", arrayOf(Atom("x")))
                    val fact2 = CompoundTerm("foo", arrayOf(Atom("x")))
                    val rule2 = Rule(fact1, mock<Query>())

                    entryStore.assertz(fact1)
                    entryStore.assertz(fact2)
                    entryStore.assertz(rule2)

                    entryStore.findFor(fact1).toList().size shouldEqual 3

                    // ACT
                    entryStore.retractAll(fact1)

                    // ASSERT
                    entryStore.clauses.toList() should beEmpty()
                    entryStore.findFor(fact2).toList() should beEmpty()
                }

                "retract should yield unifications" - {
                    "retractFact" {
                        // SETUP
                        entryStore = implementationFactory()
                        val fact = CompoundTerm("uutqwe", arrayOf(Atom("x")))
                        val retractionFact = CompoundTerm("uutqwe", arrayOf(Variable("X")))
                        val expectedUnification = fact.unify(retractionFact)!!

                        // ACT
                        entryStore.assertz(fact)
                        val result = entryStore.retractFact(retractionFact)

                        // ASSERT
                        result.remainingTo(::ArrayList) shouldEqual listOf(expectedUnification)
                    }

                    "retract" {
                        // SETUP
                        entryStore = implementationFactory()

                        val fact           = CompoundTerm("foo", arrayOf(Atom("y")))
                        val ruleHead       = CompoundTerm("foo", arrayOf(Atom("aazwe")))
                        val retractionFact = CompoundTerm("foo", arrayOf(Variable("Z")))
                        val rule = Rule(ruleHead, mock<Query>())

                        val expectedFactUnification = fact.unify(retractionFact)
                        val expectedRuleUnification = ruleHead.unify(retractionFact)

                        entryStore.assertz(fact)
                        entryStore.assertz(rule)

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