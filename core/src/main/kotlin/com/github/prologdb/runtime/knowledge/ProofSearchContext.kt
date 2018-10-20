package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.Principal
import com.github.prologdb.async.RANDOM_PRINCIPAL
import com.github.prologdb.runtime.RandomVariableScope

data class ProofSearchContext(
    /**
     * Used to prevent race conditions between side effects of multiple
     * simultaneous proof searches.
     */
    val principal: Principal,

    val knowledgeBase: KnowledgeBase,

    val randomVariableScope: RandomVariableScope
) {
    companion object {
        fun createFor(kb: KnowledgeBase): ProofSearchContext = ProofSearchContext(RANDOM_PRINCIPAL, kb, RandomVariableScope())
    }
}