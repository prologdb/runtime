package com.github.prologdb.parser

import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.term.Number
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface ParsedTerm : Term {
    val location: SourceLocationRange
}

class ParsedAtom(name: String, override val location: SourceLocationRange): ParsedTerm, Atom(name)
class ParsedList(givenElements: List<Term>, tail: ParsedTerm?, override val location: SourceLocationRange) : ParsedTerm, com.github.prologdb.runtime.term.List(givenElements, tail)
open class ParsedPredicate(name: String, arguments: Array<out ParsedTerm>, override val location: SourceLocationRange) : ParsedTerm, Predicate(name, arguments) {
    override val arguments: Array<out ParsedTerm> = arguments
}
class ParsedVariable(name: String, override val location: SourceLocationRange) : ParsedTerm, Variable(name)
interface ParsedNumber : ParsedTerm, Number
class ParsedInteger(value: Long, override val location: SourceLocationRange) : ParsedNumber, Integer(value)
class ParsedDecimal(value: Double, override val location: SourceLocationRange) : ParsedNumber, Decimal(value)

interface ParsedQuery : Query {
    val location: SourceLocationRange
}

class ParsedPredicateQuery(predicate: ParsedPredicate) : PredicateQuery(predicate), ParsedQuery {
    override val location = predicate.location
}

class ParsedAndQuery(goals: Array<out ParsedQuery>, override val location: SourceLocationRange) : AndQuery(goals), ParsedQuery
class ParsedOrQuery(goals: Array<out ParsedQuery>, override val location: SourceLocationRange) : OrQuery(goals), ParsedQuery
class EmptyQuery(override val location: SourceLocationRange): ParsedQuery {
    override fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket, randomVarsScope: RandomVariableScope): Sequence<Unification> {
        return emptySequence()
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return this
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return this
    }
}

class ParsedRule(head: ParsedPredicate, query: ParsedQuery, val location: SourceLocationRange): Rule(head, query)