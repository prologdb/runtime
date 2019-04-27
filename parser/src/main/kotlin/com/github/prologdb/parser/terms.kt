package com.github.prologdb.parser

import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.HasPrologSource
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification

class ParsedAtom(name: String, val quoted: Boolean, override val sourceInformation: SourceLocationRange): Term, HasPrologSource, Atom(name)
class ParsedList(givenElements: List<Term>, tail: Term?, override val sourceInformation: SourceLocationRange) : Term, HasPrologSource, PrologList(givenElements, tail) {
    override val elements: List<Term> = {
        val elements = mutableListOf<Term>()
        elements.addAll(givenElements)
        var pivot: Term? = tail
        while (pivot is ParsedList) {
            elements.addAll(pivot.elements)
            pivot = pivot.tail
        }
        elements
    }()
}
open class ParsedCompoundTerm(
    name: String,
    arguments: Array<out Term>,
    override val sourceInformation: SourceLocationRange
) : HasPrologSource, CompoundTerm(name, arguments) {
    /**
     * Is set to true if this was directly surrounded by parenthesis. Should prevent this compound from being
     * destructured in a compound-to-query conversion
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
            pivot = pivot.tail
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

    override fun equals(other: Any?): Boolean = AnonymousVariable == other

    override fun hashCode(): Int = AnonymousVariable.hashCode()
}

class ParsedPrologString(
    stringContent: String,
    override val sourceInformation: SourceLocationRange
) : HasPrologSource, PrologString(stringContent)

class ParsedPredicateInvocationQuery(goal: ParsedCompoundTerm) : PredicateInvocationQuery(goal), HasPrologSource {
    override val sourceInformation = goal.sourceInformation
}

class ParsedAndQuery(goals: Array<out Query>, override val sourceInformation: SourceLocationRange) : AndQuery(goals), HasPrologSource
class ParsedOrQuery(goals: Array<out Query>, override val sourceInformation: SourceLocationRange) : OrQuery(goals), HasPrologSource

class ParsedRule(head: ParsedCompoundTerm, query: Query, override val sourceInformation: SourceLocationRange): Rule(head, query), HasPrologSource

class ModuleDeclaration(
    val moduleName: String,
    /**
     * The predicates to export. If null, all predicates are exported.
     */
    val exportedPredicates: Set<ClauseIndicator>? = null
)
