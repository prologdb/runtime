package com.github.prologdb.parser

import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.HasPrologSource
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification

class ParsedAtom(name: String, override val sourceInformation: SourceLocationRange): Term, HasPrologSource, Atom(name)
class ParsedList(givenElements: List<Term>, tail: Term?, override val sourceInformation: SourceLocationRange) : Term, HasPrologSource, PrologList(givenElements, tail) {
    override val elements: List<Term> = {
        val elements = mutableListOf<Term>()
        elements.addAll(givenElements)
        var pivot: Term? = tail
        while (pivot is ParsedList) {
            elements.addAll(pivot.elements)
            pivot = pivot.tail as Term? // will always succeed because pivot is a ParsedList
        }
        elements
    }()
}
open class ParsedPredicate(
    name: String,
    arguments: Array<out Term>,
    override val sourceInformation: SourceLocationRange
) : HasPrologSource, Predicate(name, arguments) {
    /**
     * Is set to true if this was directly surrounded by parenthesis. Should prevent this predicate from being
     * destructured in a predicate-to-query conversion
     */
    var parenthesisProtection: Boolean = false
}

class ParsedDictionary(givenPairs: Map<Atom, Term>, tail: Term?, override val sourceInformation: SourceLocationRange): HasPrologSource, PrologDictionary(givenPairs, tail) {
    override val pairs: Map<Atom, Term> = {
        val pairs = mutableMapOf<Atom, Term>()
        pairs.putAll(givenPairs)
        var pivot: Term? = tail
        while (pivot is ParsedDictionary) {
            pairs.putAll(pivot.pairs)
            pivot = pivot.tail as Term?
        }
        pairs
    }()
}

class ParsedVariable(name: String, override val sourceInformation: SourceLocationRange) : HasPrologSource, Variable(name)
class ParsedAnonymousVariable(override val sourceInformation: SourceLocationRange) : HasPrologSource, Variable("_") {
    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification = AnonymousVariable.unify(rhs, randomVarsScope)

    override val variables: Set<Variable> = AnonymousVariable.variables

    override fun substituteVariables(mapper: (Variable) -> Term): Term = AnonymousVariable.substituteVariables(mapper)

    override val prologTypeName: String = AnonymousVariable.prologTypeName

    override fun equals(other: Any?): Boolean = AnonymousVariable.equals(other)
}

class ParsedPrologString(
    stringContent: String,
    override val sourceInformation: SourceLocationRange
) : HasPrologSource, PrologString(stringContent)

class ParsedPredicateQuery(predicate: ParsedPredicate) : PredicateQuery(predicate), HasPrologSource {
    override val sourceInformation = predicate.sourceInformation
}

class ParsedAndQuery(goals: Array<out Query>, override val sourceInformation: SourceLocationRange) : AndQuery(goals), HasPrologSource
class ParsedOrQuery(goals: Array<out Query>, override val sourceInformation: SourceLocationRange) : OrQuery(goals), HasPrologSource

class ParsedRule(head: ParsedPredicate, query: Query, override val sourceInformation: SourceLocationRange): Rule(head, query), HasPrologSource