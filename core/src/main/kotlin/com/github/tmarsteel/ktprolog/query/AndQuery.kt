package com.github.tmarsteel.ktprolog.query

import com.github.tmarsteel.ktprolog.knowledge.KnowledgeBase
import com.github.tmarsteel.ktprolog.unification.Unification
import kotlin.coroutines.experimental.buildSequence

class AndQuery(components: Array<out Query>) : Query {
    init {
        if (components.isEmpty()) {
            throw IllegalArgumentException("A boolean AND query must have at least one component.")
        }
    }

    override fun findProofWithin(kb: KnowledgeBase): Sequence<Unification> {



        return buildSequence {

        }
    }
}