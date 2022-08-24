package com.github.prologdb.runtime.unification

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.RandomVariableScope

/**
 * A single possible way to unify two expressions; one query result.
 */
class Unification(val variableValues: VariableBucket = VariableBucket()) {

    fun combinedWith(other: Unification, randomVariableScope: RandomVariableScope): Unification {
        return Unification(variableValues.combinedWith(other.variableValues, randomVariableScope))
    }

    /**
     * @see VariableBucket.compact
     */
    fun compact(randomVariableScope: RandomVariableScope): Unification {
        val compactedVars = variableValues.compact(randomVariableScope)
        if (compactedVars === variableValues) {
            return this
        }

        return Unification(compactedVars)
    }

    override fun toString(): String {
        return variableValues.values.joinToString(", ") { (variable, value) ->
            "$variable = $value"
        }
    }

    override fun equals(other: Any?): Boolean {
        if (other === null) return false
        if (this === other) return true

        other as Unification

        if (variableValues != other.variableValues) return false

        return true
    }

    override fun hashCode(): Int {
        return variableValues.hashCode()
    }


    companion object {
        val FALSE: Unification? = null
        val TRUE: Unification get() = Unification()
        val NONE: LazySequence<Unification> = LazySequence.empty()

        fun whether(condition: Boolean): Unification? = if(condition) TRUE else FALSE
    }
}