package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface ProofSearchContext {
    /**
     * Used to prevent race conditions between side effects of multiple
     * simultaneous proof searches.
     */
    val principal: Principal

    /**
     * To be used by code doing work in the proof search to prevent
     * collisions between variables on different stackframes.
     */
    val randomVariableScope: RandomVariableScope

    /**
     * Starts a proof search on the given query. This method will take control of
     * the coroutine until the proof search is complete.
     */
    val fulfillAttach: suspend LazySequenceBuilder<Unification>.(Query, initialVariables: VariableBucket) -> Unit
}