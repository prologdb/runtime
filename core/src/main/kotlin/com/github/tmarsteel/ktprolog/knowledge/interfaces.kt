package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.query.Query
import com.github.tmarsteel.ktprolog.term.Predicate

interface KnowledgeBase {

    fun fulfill(predicate: Predicate): Sequence<Unification>
    fun fulfill(query: Query): Sequence<Unification> = query.findProofWithin(this)

    companion object {
        val EMPTY = EmptyKnowledgeBase()
    }
}

interface MutableKnowledgeBase : KnowledgeBase {
    fun assert(predicate: Predicate)
    fun defineRule(rule: Rule)
}

class EmptyKnowledgeBase : KnowledgeBase {
    override fun fulfill(predicate: Predicate) = Unification.NONE
}