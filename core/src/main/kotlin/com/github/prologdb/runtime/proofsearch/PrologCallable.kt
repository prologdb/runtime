package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.HasFunctorAndArity
import com.github.prologdb.runtime.term.Term

typealias PrologCallableFulfill = suspend LazySequenceBuilder<Unification>.(Array<out Term>, ProofSearchContext) -> Unification?

/**
 * Something that can be called in prolog.
 */
interface PrologCallable : HasFunctorAndArity {
    /**
     * Calls this callable with the given arguments. Yields the results on the given receiver.
     */
    val fulfill: PrologCallableFulfill
}
