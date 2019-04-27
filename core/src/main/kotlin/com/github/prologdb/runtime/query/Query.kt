package com.github.prologdb.runtime.query

import com.github.prologdb.runtime.HasPrologSource
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.builtin.prologSourceInformation
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.VariableBucket
import mapToArray

sealed class Query {
    /** All the variables in this query */
    abstract val variables: Set<Variable>

    abstract fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query

    abstract fun substituteVariables(variableValues: VariableBucket): Query
}

open class AndQuery(val goals: Array<out Query>) : Query() {
    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return AndQuery(
            goals.mapToArray { it.withRandomVariables(randomVarsScope, mapping) }
        )
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return AndQuery(
            goals.mapToArray { it.substituteVariables(variableValues) }
        )
    }

    override fun toString(): String {
        return goals.mapToArray { it.toString() }.joinToString(", ")
    }

    override val variables: Set<Variable> by lazy { goals.flatMap { it.variables }.toSet() }
}

open class OrQuery(val goals: Array<out Query>) : Query() {
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

    override val variables: Set<Variable> by lazy { goals.flatMap { it.variables }.toSet() }
}

open class PredicateInvocationQuery(
    val goal: CompoundTerm,
    override val sourceInformation: PrologSourceInformation
) : Query(), HasPrologSource
{
    constructor(goal: CompoundTerm) : this(goal, getInvocationStackFrame().prologSourceInformation)

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return PredicateInvocationQuery(
            randomVarsScope.withRandomVariables(goal, mapping) as CompoundTerm,
            sourceInformation
        )
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return PredicateInvocationQuery(
            goal.substituteVariables(variableValues.asSubstitutionMapper()),
            sourceInformation
        )
    }

    override fun toString(): String {
        return goal.toString()
    }

    override val variables: Set<Variable>
        get() = goal.variables
}