package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.knowledge.library.EmptyOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.knowledge.library.MutableOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorRegistry
import com.github.tmarsteel.ktprolog.query.Query
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.unification.Unification

interface KnowledgeBase {

    fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope = RandomVariableScope()): Sequence<Unification>

    fun fulfill(query: Query, randomVarsScope: RandomVariableScope = RandomVariableScope()): Sequence<Unification> = query.findProofWithin(kb = this, randomVarsScope = randomVarsScope)

    val operatorRegistry: OperatorRegistry

    companion object {
        val EMPTY = EmptyKnowledgeBase()
    }
}

interface MutableKnowledgeBase : KnowledgeBase {
    fun assert(predicate: Predicate)
    fun defineRule(rule: Rule)
    fun load(library: Library)

    override val operatorRegistry: MutableOperatorRegistry
}

class EmptyKnowledgeBase : KnowledgeBase {
    override fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope) = Unification.NONE
    override val operatorRegistry = EmptyOperatorRegistry
}