package com.github.prologdb.runtime.stdlib.aggregate

import com.github.prologdb.async.WorkableFuture
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification

interface Reduction {
    fun add(element: Unification): WorkableFuture<Unit>
    fun finalize(): WorkableFuture<Term>
}