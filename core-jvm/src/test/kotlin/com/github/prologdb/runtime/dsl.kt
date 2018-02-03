package com.github.prologdb.runtime

import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.lazysequence.find
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

typealias UnificationSequenceGenerator = () -> LazySequence<Unification>

infix fun Term.shouldUnifyWith(rhs: Term): UnificationSequenceGenerator {
    val unification: Unification = this.unify(rhs) ?: throw AssertionError("$this should unify with $rhs but does not")
    return { LazySequence.of(unification) }
}

infix fun UnificationSequenceGenerator.suchThat(asserter: UnificationSequenceAssertionReceiver.() -> Unit) {
    UnificationSequenceAssertionReceiver(this).asserter()
}

class UnificationSequenceAssertionReceiver(private val generator: UnificationSequenceGenerator) {
    fun itHasANumberOfSolutionsIn(range: IntRange) {
        val sequence = generator()
        for (i in 0 until range.start) {
            if (sequence.tryAdvance() == null) {
                throw AssertionError("Too few unifications: expected at least ${range.start} but got only $i.")
            }
        }

        if (range.endInclusive > range.start) {
            var foundAtLeastOne = false
            for (i in range.start until range.endInclusive) {
                if (sequence.tryAdvance() == null) {
                    break
                }
                foundAtLeastOne = true
            }

            if (!foundAtLeastOne) {
                throw AssertionError("Too few unifications: expected at least ${range.start} but got only ${range.start}.")
            }
        }

        if (sequence.tryAdvance() != null) {
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
        if (generator().tryAdvance() != null) {
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
    this.fulfill(predicate).tryAdvance() ?: throw AssertionError("Failed to fulfill $predicate using knowledge base $this")

    return { this.fulfill(predicate) }
}

infix fun KnowledgeBase.shouldProve(query: Query): UnificationSequenceGenerator {
    this.fulfill(query).tryAdvance() ?: throw AssertionError("Failed to fulfill $query using knowledge base $this")
    return { this.fulfill(query) }
}

infix fun KnowledgeBase.shouldNotProve(query: Query) {
    if (this.fulfill(query).tryAdvance() != null) {
        throw AssertionError("$this should not fulfill $query but does.")
    }
}

infix fun KnowledgeBase.shouldNotProve(predicate: Predicate) {
    if (this.fulfill(predicate).tryAdvance() != null) {
        throw AssertionError("$this should not fulfill $predicate but does.")
    }
}

private fun <T> LazySequence<T>.any(predicate: (T) -> Boolean): Boolean
    = find(predicate) != null