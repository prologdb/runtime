package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

interface Clause : HasNameAndArity {
    /**
     * Unifies the given predicate (`other`) with this entry; if this is a fact (a [Predicate]), unifies with
     * the given predicate and ignores the given [KnowledgeBase]. If this is a rule, uses the [KnowledgeBase]
     * to run the query (in case the head and the given [Predicate] unify).
     */
    val unifyWithKnowledge: suspend LazySequenceBuilder<Unification>.(other: Predicate, context: ProofSearchContext) -> Unit
}