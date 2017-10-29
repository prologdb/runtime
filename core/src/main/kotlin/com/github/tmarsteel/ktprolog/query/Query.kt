package com.github.tmarsteel.ktprolog.query

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.VariableMapping
import com.github.tmarsteel.ktprolog.knowledge.KnowledgeBase
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.unification.VariableBucket

interface Query {
    fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket = VariableBucket(),
                        randomVarsScope: RandomVariableScope = RandomVariableScope()): Sequence<Unification>

    fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query

    fun substituteVariables(variableValues: VariableBucket): Query
}