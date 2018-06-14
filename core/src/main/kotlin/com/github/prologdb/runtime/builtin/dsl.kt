/**
 * Simple DSL for notating prolog in kotlin within prologdb for the
 * purpose of declaring builtins. Might be made public some day.
 */
package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.AnonymousVariable
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.Decimal as PrologDecimal
import com.github.prologdb.runtime.term.Integer as PrologInteger
import com.github.prologdb.runtime.term.List as PrologList
import com.github.prologdb.runtime.term.Number as PrologNumber

/**
 * Creates a new [Predicate] with [this] as the name and the
 * given arguments
 */
operator fun String.invoke(vararg args: Term) = Predicate(this, args)

/**
 * Creates a new [Variable] with the given name
 */
fun V(name: String) = if (name == "_") AnonymousVariable else Variable(name)

/**
 * Creates a new prolog list of the given elements without a tail.
 *
 * @see
 */
fun L(vararg elements: Term): PrologList = PrologList(elements.toList())

/**
 * Creates a new prolog number from the given kotlin number
 */
fun N(number: Number): PrologNumber {
    if (number is Byte || number is Short || number is Int || number is Long) {
        return PrologInteger(number.toLong())
    } else {
        return PrologDecimal(number.toDouble())
    }
}

/**
 * Creates a new [Rule] with the receiver as the [Rule.head]; obtains
 * the body from the given lambda.
 */
operator fun Predicate.invoke(definition: () -> Query) = Rule(this, definition())

/**
 * Creates a new prolog list as a copy of the receiver with the given term
 * as the tail.
 *
 * @throws IllegalStateException If the receiver already has a tail
 */
fun PrologList.tail(tail: Term): PrologList {
    if (this.tail != null) throw IllegalStateException("The list already has a tail (${this.tail}), cannot set $tail as the tail")

    return PrologList(this.elements, tail)
}

// these functions model the logic operators in all the different combinations

infix fun Predicate.AND(rhs: Predicate): Query = AndQuery(arrayOf(
    PredicateQuery(this),
    PredicateQuery(this)
))

infix fun Predicate.AND(rhs: Query): Query = rhs.AND(this)

infix fun Query.AND(rhs: Predicate): Query {
    if (this is AndQuery) {
        return AndQuery(Array(this.goals.size + 1, { index ->
            if (index < this.goals.size) this.goals[index] else PredicateQuery(rhs)
        }))
    } else {
        return AndQuery(arrayOf(this, PredicateQuery(rhs)))
    }
}

infix fun Predicate.OR(rhs: Predicate): Query = OrQuery(arrayOf(
    PredicateQuery(this),
    PredicateQuery(this)
))

infix fun Predicate.OR(rhs: Query): Query = rhs.OR(this)

infix fun Query.OR(rhs: Predicate): Query {
    if (this is OrQuery) {
        return OrQuery(Array(this.goals.size + 1, { index ->
            if (index < this.goals.size) this.goals[index] else PredicateQuery(rhs)
        }))
    } else {
        return OrQuery(arrayOf(this, PredicateQuery(rhs)))
    }
}