package com.github.prologdb.runtime.query

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import mapToArray
import kotlin.coroutines.experimental.buildSequence

open class OrQuery(val goals: Array<out Query>) : Query {
    override fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket, randomVarsScope: RandomVariableScope): Sequence<Unification> {
        return buildSequence {
            for (goal in goals) {
                yieldAll(goal.findProofWithin(kb, initialVariables, randomVarsScope))
            }
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