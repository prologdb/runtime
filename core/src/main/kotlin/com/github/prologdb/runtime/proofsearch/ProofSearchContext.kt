package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import com.github.prologdb.runtime.util.OperatorRegistry

typealias ProofSearch = suspend LazySequenceBuilder<Unification>.(Query, initialVariables: VariableBucket) -> Unification?

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

    /**
     * The modules available in the root runtime environment.
     */
    val rootAvailableModules: Map<String, Module>

    val operators: OperatorRegistry

    /**
     * Starts a proof search on the given query. Solutions will be [LazySequenceBuilder.yield]ed onto the
     * given [LazySequenceBuilder]. This method will remain in control of the coroutine until execution of
     * the query has finished.
     */
    val fulfillAttach: ProofSearch

    /**
     * Resolves the given, context-sensitive indicator against the contexts imports
     * to the [PrologPredicate] it currently refers to.
     */
    fun resolveCallable(simpleIndicator: ClauseIndicator): Pair<FullyQualifiedClauseIndicator, PrologCallable>?

    /**
     * @return if goal is an instance of `:/2` and the referred predicate exists
     * and is callable: first: the fqn of the referred callable, second: the callable
     * third: the invocation arguments
     */
    fun resolveModuleScopedCallable(goal: Clause): Triple<FullyQualifiedClauseIndicator, PrologCallable, Array<out Term>>?
}
