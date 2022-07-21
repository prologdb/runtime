package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PredicateNotDefinedException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
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
    @Throws(PredicateNotDefinedException::class)
    fun resolveCallable(simpleIndicator: ClauseIndicator): Pair<FullyQualifiedClauseIndicator, PrologCallable>

    /**
     * @return if goal is an instance of `:/2` and the referred predicate exists
     * and is callable: first: the fqn of the referred callable, second: the callable
     * third: the invocation arguments
     */
    @Throws(PredicateNotDefinedException::class)
    fun resolveModuleScopedCallable(goal: Clause): Triple<FullyQualifiedClauseIndicator, PrologCallable, Array<out Term>>?

    /**
     * @return a [ProofSearchContext] where [resolveCallable] and [resolveHead] work in the context of the
     * module with the given name. The module must already have been loaded.
     */
    fun deriveForModuleContext(moduleName: String): ProofSearchContext

    /**
     * If `head` is an instance of `:/2` (module-qualified), the explicitly
     * given module will be searched for the predicate. If not, this modules
     * context will be searched, including import aliases.
     * @return first: the fully qualified indicator, without any aliases,
     * second: the predicate itself, third: the given `head` term but the functor
     * is assured to be the actual name of the predicate (relevant for local lookups with aliases).
     */
    @Throws(PredicateNotDefinedException::class)
    fun resolveHead(head: CompoundTerm): Triple<FullyQualifiedClauseIndicator, PrologCallable, CompoundTerm> {
        resolveModuleScopedCallable(head)?.let { fqTerm ->
            return Triple(fqTerm.first, fqTerm.second, CompoundTerm(fqTerm.second.functor, fqTerm.third))
        }

        val simpleIndicator = ClauseIndicator.of(head)

        val resolved = resolveCallable(simpleIndicator)
        val fqIndicator = resolved.first

        return Triple(
            fqIndicator,
            resolved.second,
            if (fqIndicator.indicator.functor == head.functor) head else CompoundTerm(
                fqIndicator.indicator.functor,
                head.arguments
            )
        )
    }

    companion object {
        /**
         * Convenience method for Java to do a proof search with the [fulfillAttach] coroutine
         */
        @JvmStatic
        fun fulfill(psc: ProofSearchContext, query: Query, initialVariables: VariableBucket): LazySequence<Unification> {
            return buildLazySequence(psc.principal) {
                psc.fulfillAttach(this, query, initialVariables)
            }
        }
    }
}
