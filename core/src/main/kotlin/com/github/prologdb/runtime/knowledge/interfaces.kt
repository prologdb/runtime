package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.unification.Unification

interface KnowledgeBase {
    val operators: OperatorRegistry

    fun fulfill(asPrincipal: Principal, query: Query, randomVariableScope: RandomVariableScope): LazySequence<Unification>

    fun load(library: Library)
}