package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
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

    val authorization: Authorization

    val runtime: PrologRuntimeEnvironment

    /**
     * Starts a proof search on the given query. Solutions will be [LazySequenceBuilder.yield]ed onto the
     * given [LazySequenceBuilder]. This method will remain in control of the coroutine until execution of
     * the query has finished.
     */
    val fulfillAttach: suspend LazySequenceBuilder<Unification>.(Query, initialVariables: VariableBucket) -> Unit

    /**
     * Resolves the given, context-sensitive indicator against the contexts imports
     * to the [PrologPredicate] it currently refers to.
     */
    fun resolveCallable(simpleIndicator: ClauseIndicator): Pair<FullyQualifiedClauseIndicator, PrologCallable>?
}
