package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.find
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.ReadWriteAuthorization
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

typealias UnificationGenerator = () -> Unification
typealias UnificationSequenceGenerator = () -> LazySequence<Unification>

infix fun Term.shouldUnifyWith(rhs: Term): UnificationGenerator {
    val unification: Unification = this.unify(rhs) ?: throw AssertionError("$this should unify with $rhs but does not")
    return { unification }
}

@JvmName("suchThatMany")
infix fun UnificationSequenceGenerator.suchThat(asserter: UnificationSequenceAssertionReceiver.() -> Unit) {
    UnificationSequenceAssertionReceiver(this).asserter()
}

@JvmName("suchThatSingle")
infix fun UnificationGenerator.suchThat(asserter: (Unification) -> Boolean) {
    if (!asserter(this())) {
        throw AssertionError()
    }
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
    return shouldProve(PredicateQuery(predicate))
}

infix fun KnowledgeBase.shouldProve(query: Query): UnificationSequenceGenerator {
    val sequence = this.fulfill(query, ReadWriteAuthorization)
    val firstSolution = sequence.tryAdvance()
    sequence.close()

    if (firstSolution == null) throw AssertionError("Failed to fulfill $query using knowledge base $this")

    return { this.fulfill(query, ReadWriteAuthorization) }
}

infix fun KnowledgeBase.shouldNotProve(query: Query) {
    val solutions = this.fulfill(query, ReadWriteAuthorization)
    val firstSolution = solutions.tryAdvance()
    solutions.close()

    if (firstSolution != null) {
        throw AssertionError("$this should not fulfill $query but does.")
    }
}

infix fun KnowledgeBase.shouldNotProve(predicate: Predicate) {
    shouldNotProve(PredicateQuery(predicate))
}

private fun <T> LazySequence<T>.any(predicate: (T) -> Boolean): Boolean
    = find(predicate) != null