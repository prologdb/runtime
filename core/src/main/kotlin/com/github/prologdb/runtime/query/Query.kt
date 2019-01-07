package com.github.prologdb.runtime.query

import com.github.prologdb.runtime.*
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

open class PredicateQuery(
        val predicate: CompoundTerm,
        override val sourceInformation: PrologSourceInformation
) : Query(), HasPrologSource
{
    constructor(predicate: CompoundTerm) : this(predicate, getInvocationStackFrame().prologSourceInformation)

    /**
     * The stack frame to use in exceptions for this query.
     */
    val stackFrame: PrologStackTraceElement by lazy {
        PrologStackTraceElement(
            predicate,
            sourceInformation
        )
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return PredicateQuery(
            randomVarsScope.withRandomVariables(predicate, mapping) as CompoundTerm,
            sourceInformation
        )
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return PredicateQuery(
            predicate.substituteVariables(variableValues.asSubstitutionMapper()),
            sourceInformation
        )
    }

    override fun toString(): String {
        return predicate.toString()
    }

    override val variables: Set<Variable>
        get() = predicate.variables
}