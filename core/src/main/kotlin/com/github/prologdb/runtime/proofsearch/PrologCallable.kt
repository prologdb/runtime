package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.HasFunctorAndArity
import com.github.prologdb.runtime.analyzation.constraint.DeterminismLevel
import com.github.prologdb.runtime.analyzation.constraint.InvocationConstraint
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
     * @return the conditions under which the receiver behaves according to the given [DeterminismLevel]. An empty list
     * indicates the the receiver will, under no circumstances, behave that way. Null indicates that the constraints could
     * not be determined (see halting problem).
     * TODO: find a more suitable name for this
     */
    fun conditionsForBehaviour(level: DeterminismLevel): List<InvocationConstraint>?
}