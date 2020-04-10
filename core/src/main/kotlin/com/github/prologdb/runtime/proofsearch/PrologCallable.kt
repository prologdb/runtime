package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.HasFunctorAndArity
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.analyzation.constraint.DeterminismLevel
import com.github.prologdb.runtime.analyzation.constraint.InvocationBehaviour
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

/**
 * Something that can be called in prolog.
 */
interface PrologCallable : HasFunctorAndArity {
    /**
     * Calls this callable with the given arguments. Yields the results on the given receiver.
     */
    val fulfill: suspend LazySequenceBuilder<Unification>.(Array<out Term>, ProofSearchContext) -> Unit
}

/**
 * A [PrologCallable] that can delegate calls to its [fulfill] method
 * to another [PrologCallable] if desired. Used for optimization and jitting.
 */
interface DelegatableCallable : PrologCallable {
    /**
     * Sets the delegate to be used.
     * @param dropOnModification If true, the delegate should be dropped (see [dropDelegate]) when the
     *                           predicate is modified.
     */
    fun setDelegate(delegate: PrologCallable, dropOnModification: Boolean)

    /**
     * Removes the delegation until the next invocation of [setDelegate]
     */
    fun dropDelegate()
}

interface BehaviourExposingPrologCallable : PrologCallable {
    /**
     * @return the behaviours to be expected from `this`' [PrologCallable.fulfill] that are categorized as [DeterminismLevel].
     * An empty list indicates the the receiver will, under no circumstances, behave that way.
     * Null indicates that the behaviours could not be reliably determined (see halting problem).
     */
    fun getBehaviours(inRuntime: PrologRuntimeEnvironment, callingModule: Module, level: DeterminismLevel): List<InvocationBehaviour>?
}