package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.knowledge.library.EmptyOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.MutableOperatorRegistry
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface KnowledgeBase {

    fun fulfill(predicate: Predicate, context: ProofSearchContext = ProofSearchContext.createFor(this)): LazySequence<Unification>

    fun fulfill(query: Query, context: ProofSearchContext = ProofSearchContext.createFor(this)): LazySequence<Unification> {
        if (context.knowledgeBase != this) throw IllegalArgumentException("Given context must belong to the same knowledge base")

        return buildLazySequence(context.principal) {
            query.findProofWithin(this, context, VariableBucket())
        }
    }

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
    override fun fulfill(predicate: Predicate, context: ProofSearchContext) = Unification.NONE
    override val operatorRegistry = EmptyOperatorRegistry
}