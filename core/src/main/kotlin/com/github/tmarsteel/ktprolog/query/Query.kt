package com.github.tmarsteel.ktprolog.query

import com.github.tmarsteel.ktprolog.knowledge.KnowledgeBase
import com.github.tmarsteel.ktprolog.unification.Unification
import kotlin.coroutines.experimental.buildSequence

interface Query {
    fun findProofWithin(kb: KnowledgeBase): Sequence<Unification>
}