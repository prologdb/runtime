package com.github.prologdb.runtime.optimization

import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.PrologPredicate

class TailcallOptimizer {

    /**
     * Attempts to optimize a tailcall in the given predicate.
     *
     * @return A [PrologPredicate] identical behaviour as the given predicate and
     *         with tail call optimization applied. Returns `null` if the given predicate
     *         cannot be optimized.
     */
    fun tryOptimize(predicate: ASTPrologPredicate): PrologPredicate? {
        return null // TODO
    }
}
