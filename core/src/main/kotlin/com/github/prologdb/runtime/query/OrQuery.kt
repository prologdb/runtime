package com.github.prologdb.runtime.query

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import mapToArray

open class OrQuery(val goals: Array<out Query>) : Query {
    override val findProofWithin: suspend LazySequenceBuilder<Unification>.(ProofSearchContext, VariableBucket) -> Unit = { context, initialVariables ->
        for (goal in goals) {
            goal.findProofWithin(this, context, initialVariables)
        }
    }

    override fun toString(): String {
        return goals.mapToArray { it.toString() }.joinToString(" ; ")
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return OrQuery(
            goals.mapToArray { it.withRandomVariables(randomVarsScope, mapping) }
        )
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return OrQuery(
            goals.mapToArray { it.substituteVariables(variableValues) }
        )
    }
}