package com.github.prologdb.runtime.query

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.builtin.prologSourceInformation
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.util.OperatorRegistry
import mapToArray

sealed class Query {
    /** All the variables in this query */
    abstract val variables: Set<Variable>

    abstract fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query

    abstract fun substituteVariables(variableValues: Unification): Query

    /** From where this term was parsed. Set to [com.github.prologdb.runtime.NullSourceInformation] if unavailable. */
    var sourceInformation: PrologSourceInformation = NullSourceInformation

    abstract fun toStringUsingOperatorNotation(operators: OperatorRegistry, indent: String = ""): String
}

class AndQuery(val goals: Array<out Query>) : Query() {
    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return AndQuery(
            goals.mapToArray { it.withRandomVariables(randomVarsScope, mapping) }
        )
    }

    override fun substituteVariables(variableValues: Unification): Query {
        return AndQuery(
            goals.mapToArray { it.substituteVariables(variableValues) }
        )
    }

    override fun toString(): String {
        return goals.mapToArray { it.toString() }.joinToString(", ")
    }

    override val variables: Set<Variable> by lazy { goals.flatMap { it.variables }.toSet() }

    override fun toStringUsingOperatorNotation(operators: OperatorRegistry, indent: String): String = goals.joinToString(
            transform = { it.toStringUsingOperatorNotation(operators, "$indent  ") },
            separator = "\n$indent,\n"
        )
}

class OrQuery(val goals: Array<out Query>) : Query() {
    override fun toString(): String {
        return goals.mapToArray { it.toString() }.joinToString(" ; ")
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return OrQuery(
            goals.mapToArray { it.withRandomVariables(randomVarsScope, mapping) }
        )
    }

    override fun substituteVariables(variableValues: Unification): Query {
        return OrQuery(
            goals.mapToArray { it.substituteVariables(variableValues) }
        )
    }

    override val variables: Set<Variable> by lazy { goals.flatMap { it.variables }.toSet() }

    override fun toStringUsingOperatorNotation(operators: OperatorRegistry, indent: String): String = goals.joinToString(
        transform = { it.toStringUsingOperatorNotation(operators, "$indent  ") },
        separator = "\n$indent;\n"
    )
}

class PredicateInvocationQuery(
    val goal: CompoundTerm,
    sourceInformation: PrologSourceInformation
) : Query() {
    constructor(goal: CompoundTerm) : this(goal, getInvocationStackFrame().prologSourceInformation)

    init {
        this.sourceInformation = sourceInformation
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return PredicateInvocationQuery(
            randomVarsScope.withRandomVariables(goal, mapping),
            sourceInformation,
        )
    }

    override fun substituteVariables(variableValues: Unification): Query {
        return PredicateInvocationQuery(
            goal.substituteVariables(variableValues.asSubstitutionMapper()),
            sourceInformation,
        )
    }

    override fun toString(): String {
        return goal.toString()
    }

    override val variables: Set<Variable>
        get() = goal.variables

    override fun toStringUsingOperatorNotation(operators: OperatorRegistry, indent: String): String = indent + goal.toStringUsingOperatorNotations(operators)
}