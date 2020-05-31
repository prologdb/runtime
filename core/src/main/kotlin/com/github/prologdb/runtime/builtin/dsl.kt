/**
 * Simple DSL for notating prolog in kotlin within prologdb for the
 * purpose of declaring builtins. Might be made public some day.
 */
package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDecimal
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

/**
 * Creates a new [CompoundTerm] with [this] as the functor and the
 * given arguments
 */
operator fun String.invoke(vararg args: Term) = CompoundTerm(this, args)

/**
 * Creates a new [Variable] with the given name
 */
fun V(name: String) = if (name == "_") Variable.ANONYMOUS else Variable(name)

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
operator fun CompoundTerm.invoke(definition: () -> Query) = Rule(this, definition())

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

infix fun CompoundTerm.AND(rhs: CompoundTerm): Query {
    val callerSourceInfo = getInvocationStackFrame().prologSourceInformation

    return AndQuery(arrayOf(
        PredicateInvocationQuery(this, callerSourceInfo),
        PredicateInvocationQuery(rhs, callerSourceInfo)
    ))
}

infix fun CompoundTerm.AND(rhs: Query): Query {
    val callerSourceInfo = getInvocationStackFrame().prologSourceInformation

    return AndQuery(arrayOf(
        PredicateInvocationQuery(this, callerSourceInfo),
        rhs
    ))
}

infix fun Query.AND(rhs: CompoundTerm): Query {
    val callerSourceInfo = getInvocationStackFrame().prologSourceInformation
    if (this is AndQuery) {
        return AndQuery(
            Array(this.goals.size + 1, { index ->
                if (index < this.goals.size) this.goals[index] else PredicateInvocationQuery(rhs, callerSourceInfo)
            })
        )
    } else {

        return AndQuery(arrayOf(this, PredicateInvocationQuery(rhs, callerSourceInfo)))
    }
}

infix fun CompoundTerm.OR(rhs: CompoundTerm): Query {
    val callerSourceInfo = getInvocationStackFrame().prologSourceInformation

    return OrQuery(arrayOf(
        PredicateInvocationQuery(this, callerSourceInfo),
        PredicateInvocationQuery(rhs, callerSourceInfo)
    ))
}

infix fun CompoundTerm.OR(rhs: Query): Query {
    val callerSourceInfo = getInvocationStackFrame().prologSourceInformation

    return OrQuery(arrayOf(
        PredicateInvocationQuery(this, callerSourceInfo),
        rhs
    ))
}

infix fun Query.OR(rhs: CompoundTerm): Query {
    val callerSourceInfo = getInvocationStackFrame().prologSourceInformation

    if (this is OrQuery) {
        return OrQuery(Array(this.goals.size + 1, { index ->
            if (index < this.goals.size) this.goals[index] else PredicateInvocationQuery(rhs, callerSourceInfo)
        }))
    } else {
        return OrQuery(arrayOf(this, PredicateInvocationQuery(rhs, callerSourceInfo)))
    }
}