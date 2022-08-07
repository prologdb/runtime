package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.WorkableFuture
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.VariableBucket

interface Reduction {
    suspend fun add(element: VariableBucket): WorkableFuture<Unit>
    suspend fun finalize(): WorkableFuture<Term>
}