package com.github.tmarsteel.ktprolog

import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.knowledge.KnowledgeBase
import com.github.tmarsteel.ktprolog.query.Query
import com.github.tmarsteel.ktprolog.term.Predicate

typealias UnificationSequenceGenerator = () -> Sequence<Unification>

infix fun Term.shouldUnifyWith(rhs: Term): UnificationSequenceGenerator {
    val unification: Unification = this.unify(rhs) ?: throw AssertionError("$this should unify with $rhs but does not")
    return { setOf(unification).asSequence() }
}

infix fun UnificationSequenceGenerator.suchThat(asserter: UnificationSequenceAssertionReceiver.() -> Unit) {
    UnificationSequenceAssertionReceiver(this).asserter()
}

class UnificationSequenceAssertionReceiver(private val generator: UnificationSequenceGenerator) {
    fun itHasANumberOfSolutionsIn(range: IntRange) {
        val iterator = generator().iterator()
        for (i in 0 until range.start) {
            if (!iterator.hasNext()) {
                throw AssertionError("Too few unifications: expected at least ${range.start} but got only $i.")
            }
            iterator.next()
        }

        if (range.endInclusive > range.start) {
            var foundAtLeastOne = false
            for (i in range.start until range.endInclusive) {
                if (!iterator.hasNext()) {
                    break
                }
                foundAtLeastOne = true
                iterator.next()
            }

            if (!foundAtLeastOne) {
                throw AssertionError("Too few unifications: expected at least ${range.start} but got only ${range.start}.")
            }
        }

        if (iterator.hasNext()) {
            throw AssertionError("Too many unifications: expected at most ${range.endInclusive} but got more (at least ${range.endInclusive + 1} found)")
        }
    }

    fun itHasExactlyNSolutions(n: Int) {
        itHasANumberOfSolutionsIn(IntRange(n, n))
    }

    fun itHasExactlyOneSolution() {
        itHasExactlyNSolutions(1)
    }

    fun itHasASolutionSuchThat(description: String, predicate: (Unification) -> Boolean) {
        if (!generator().any(predicate)) {
            throw AssertionError("There is no solution such that $description")
        }
    }

    fun itHasNoSolutions() {
        if (generator().any()) {
            throw AssertionError("A solution was found when there should have been none.")
        }
    }
}

infix fun Term.shouldNotUnifyWith(rhs: Term) {
    val unification = this.unify(rhs)

    if (unification != null) {
        throw AssertionError("$this should not unify with $rhs but does")
    }
}

infix fun KnowledgeBase.shouldProve(predicate: Predicate): UnificationSequenceGenerator {
    try {
        this.fulfill(predicate).first()
        return { this.fulfill(predicate) }
    }
    catch (ex: NoSuchElementException) {
        throw AssertionError("Failed to fulfill $predicate using knowledge base $this")
    }
}

infix fun KnowledgeBase.shouldProve(query: Query): UnificationSequenceGenerator {
    try {
        this.fulfill(query).first()
        return { this.fulfill(query) }
    }
    catch (ex: NoSuchElementException) {
        throw AssertionError("Failed to fulfill $query using knowledge base $this")
    }
}

infix fun KnowledgeBase.shouldNotProve(query: Query) {
    try {
        this.fulfill(query).first()
        throw AssertionError("$this should not fulfill $query but does.")
    }
    catch (ex: NoSuchElementException) {
        // all good!
    }
}

infix fun KnowledgeBase.shouldNotProve(predicate: Predicate) {
    try {
        this.fulfill(predicate).first()
        throw AssertionError("$this should not fulfill $predicate but does.")
    }
    catch (ex: NoSuchElementException) {
        // all good!
    }
}