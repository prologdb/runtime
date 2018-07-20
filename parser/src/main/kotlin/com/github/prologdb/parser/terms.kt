package com.github.prologdb.parser

import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.HasPrologSource
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface ParsedTerm : Term, HasPrologSource {
    override val sourceInformation: SourceLocationRange
}

class ParsedAtom(name: String, override val sourceInformation: SourceLocationRange): ParsedTerm, Atom(name)
class ParsedList(givenElements: List<ParsedTerm>, tail: ParsedTerm?, override val sourceInformation: SourceLocationRange) : ParsedTerm, PrologList(givenElements, tail) {
    override val elements: List<ParsedTerm> = {
        val elements = mutableListOf<ParsedTerm>()
        elements.addAll(givenElements)
        var pivot: ParsedTerm? = tail
        while (pivot is ParsedList) {
            elements.addAll(pivot.elements)
            pivot = pivot.tail as ParsedTerm? // will always succeed because pivot is a ParsedList
        }
        elements
    }()
}
open class ParsedPredicate(name: String, arguments: Array<out ParsedTerm>, override val sourceInformation: SourceLocationRange) : ParsedTerm, Predicate(name, arguments) {
    override val arguments: Array<out ParsedTerm> = arguments
}

class ParsedDictionary(givenPairs: Map<Atom, ParsedTerm>, tail: ParsedTerm?, override val sourceInformation: SourceLocationRange): ParsedTerm, PrologDictionary(givenPairs, tail) {
    override val pairs: Map<Atom, ParsedTerm> = {
        val pairs = mutableMapOf<Atom, ParsedTerm>()
        pairs.putAll(givenPairs)
        var pivot: ParsedTerm? = tail
        while (pivot is ParsedDictionary) {
            pairs.putAll(pivot.pairs)
            pivot = pivot.tail as ParsedTerm?
        }
        pairs
    }()
}

class ParsedVariable(name: String, override val sourceInformation: SourceLocationRange) : ParsedTerm, Variable(name)
class ParsedAnonymousVariable(override val sourceInformation: SourceLocationRange) : ParsedTerm, Variable("_") {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification = AnonymousVariable.unify(rhs, randomVarsScope)

    override val variables: Set<Variable> = AnonymousVariable.variables

    override fun substituteVariables(mapper: (Variable) -> Term): Term = AnonymousVariable.substituteVariables(mapper)

    override val prologTypeName: String = AnonymousVariable.prologTypeName

    override fun equals(other: Any?): Boolean = AnonymousVariable.equals(other)
}

class ParsedPrologNumber(
    private val delegate: PrologNumber,
    override val sourceInformation: SourceLocationRange
) : ParsedTerm, PrologNumber {
    override val variables: Set<Variable> = delegate.variables
    override fun equals(other: Any?) = delegate.equals(other)
    override fun substituteVariables(mapper: (Variable) -> Term) = delegate.substituteVariables(mapper)
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope) = delegate.unify(rhs, randomVarsScope)
    override fun compareTo(other: PrologNumber) = delegate.compareTo(other)

    override fun plus(other: PrologNumber) = delegate.plus(other)
    override fun minus(other: PrologNumber) = delegate.minus(other)
    override fun times(other: PrologNumber) = delegate.times(other)
    override fun div(other: PrologNumber) = delegate.div(other)
    override fun rem(other: PrologNumber) = delegate.rem(other)
    override fun toThe(other: PrologNumber) = delegate.toThe(other)
    override fun unaryPlus() = delegate.unaryPlus()
    override fun unaryMinus() = delegate.unaryMinus()
    override fun toInteger() = delegate.toInteger()
    override fun toDecimal() = delegate.toDecimal()
    override val isInteger = delegate.isInteger

    override fun compareTo(other: Term) = delegate.compareTo(other)

    override fun toString() = delegate.toString()
}

class ParsedPrologString(
    stringContent: String,
    override val sourceInformation: SourceLocationRange
) : ParsedTerm, PrologString(stringContent)

interface ParsedQuery : Query, HasPrologSource {
    override val sourceInformation: SourceLocationRange
}

class ParsedPredicateQuery(predicate: ParsedPredicate) : PredicateQuery(predicate), ParsedQuery {
    override val sourceInformation = predicate.sourceInformation
}

class ParsedAndQuery(goals: Array<out ParsedQuery>, override val sourceInformation: SourceLocationRange) : AndQuery(goals), ParsedQuery
class ParsedOrQuery(goals: Array<out ParsedQuery>, override val sourceInformation: SourceLocationRange) : OrQuery(goals), ParsedQuery
class EmptyQuery(override val sourceInformation: SourceLocationRange): ParsedQuery {
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

class ParsedRule(head: ParsedPredicate, query: ParsedQuery, override val sourceInformation: SourceLocationRange): Rule(head, query), HasPrologSource