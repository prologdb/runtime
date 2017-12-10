package com.github.prologdb.runtime.query

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface Query {
    fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket = VariableBucket(),
                        randomVarsScope: RandomVariableScope = RandomVariableScope()): Sequence<Unification>

    fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query

    fun substituteVariables(variableValues: VariableBucket): Query
}