package com.github.tmarsteel.ktprolog.parser

import com.github.tmarsteel.ktprolog.parser.source.SourceLocationRange
import com.github.tmarsteel.ktprolog.term.Atom
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable

interface ParsedTerm : Term {
    val location: SourceLocationRange
}

class ParsedAtom(name: String, override val location: SourceLocationRange): ParsedTerm, Atom(name)
class ParsedList(givenElements: List<Term>, tail: Variable?, override val location: SourceLocationRange) : ParsedTerm, com.github.tmarsteel.ktprolog.term.List(givenElements, tail)
class ParsedPredicate(name: String, arguments: Array<out Term>, override val location: SourceLocationRange) : ParsedTerm, Predicate(name, arguments)
class ParsedVariable(name: String, override val location: SourceLocationRange) : ParsedTerm, Variable(name)