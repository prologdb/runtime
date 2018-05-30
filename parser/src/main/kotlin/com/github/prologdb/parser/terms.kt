package com.github.prologdb.parser

import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.List as PrologList
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.Integer as PrologInteger
import com.github.prologdb.runtime.term.Decimal as PrologDecimal
import com.github.prologdb.runtime.term.Number
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface ParsedTerm : Term {
    val location: SourceLocationRange
}

class ParsedAtom(name: String, override val location: SourceLocationRange): ParsedTerm, Atom(name)
class ParsedList(givenElements: List<Term>, tail: ParsedTerm?, override val location: SourceLocationRange) : ParsedTerm, PrologList(givenElements, tail)
open class ParsedPredicate(name: String, arguments: Array<out ParsedTerm>, override val location: SourceLocationRange) : ParsedTerm, Predicate(name, arguments) {
    override val arguments: Array<out ParsedTerm> = arguments
}
class ParsedVariable(name: String, override val location: SourceLocationRange) : ParsedTerm, Variable(name)
interface ParsedNumber : ParsedTerm, Number
class ParsedInteger(value: Long, override val location: SourceLocationRange) : ParsedNumber {
    private val delegate = PrologInteger.createUsingStringOptimizerCache(value)

    override val variables: Set<Variable> = delegate.variables
    override fun equals(other: Any?) = delegate.equals(other)
    override fun substituteVariables(mapper: (Variable) -> Term) = delegate.substituteVariables(mapper)
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope) = delegate.unify(rhs, randomVarsScope)
    override fun compareTo(other: Number) = delegate.compareTo(other)

    override fun plus(other: Number) = delegate.plus(other)
    override fun minus(other: Number) = delegate.minus(other)
    override fun times(other: Number) = delegate.times(other)
    override fun div(other: Number) = delegate.div(other)
    override fun rem(other: Number) = delegate.rem(other)
    override fun toThe(other: Number) = delegate.toThe(other)
    override fun unaryPlus() = delegate.unaryPlus()
    override fun unaryMinus() = delegate.unaryMinus()
}
class ParsedDecimal(value: Double, override val location: SourceLocationRange) : ParsedNumber, PrologDecimal(value)

interface ParsedQuery : Query {
    val location: SourceLocationRange
}

class ParsedPredicateQuery(predicate: ParsedPredicate) : PredicateQuery(predicate), ParsedQuery {
    override val location = predicate.location
}

class ParsedAndQuery(goals: Array<out ParsedQuery>, override val location: SourceLocationRange) : AndQuery(goals), ParsedQuery
class ParsedOrQuery(goals: Array<out ParsedQuery>, override val location: SourceLocationRange) : OrQuery(goals), ParsedQuery
class EmptyQuery(override val location: SourceLocationRange): ParsedQuery {
    override fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket, randomVarsScope: RandomVariableScope): LazySequence<Unification> {
        return Unification.NONE
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return this
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return this
    }
}

class ParsedRule(head: ParsedPredicate, query: ParsedQuery, val location: SourceLocationRange): Rule(head, query)