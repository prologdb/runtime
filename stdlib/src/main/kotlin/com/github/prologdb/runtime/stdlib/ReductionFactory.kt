package com.github.prologdb.runtime.stdlib

import com.github.prologdb.async.WorkableFuture
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.stdlib.aggregate.Reduction

interface ReductionFactory {
    fun create(proofSearchContext: ProofSearchContext): WorkableFuture<out Reduction>
}