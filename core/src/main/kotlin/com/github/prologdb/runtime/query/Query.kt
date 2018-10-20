package com.github.prologdb.runtime.query

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

interface Query {
    val findProofWithin: suspend LazySequenceBuilder<Unification>.(context: ProofSearchContext, initialVariables: VariableBucket) -> Unit

    fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query

    fun substituteVariables(variableValues: VariableBucket): Query
}