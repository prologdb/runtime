package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.knowledge.library.HasFunctorAndArity
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification

/**
 * Something that can be called in prolog.
 */
interface PrologCallable : HasFunctorAndArity {
    /**
     * Calls this callable with the given invocation goal. Yields the results on the given receiver.
     */
    val fulfill: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unit
}