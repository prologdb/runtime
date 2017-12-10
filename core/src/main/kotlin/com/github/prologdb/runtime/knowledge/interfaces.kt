package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.library.EmptyOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.MutableOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

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