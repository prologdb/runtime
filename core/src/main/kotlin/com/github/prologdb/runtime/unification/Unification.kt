package com.github.prologdb.runtime.unification

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.term.Variable

/**
 * A single possible way to unify two expressions; one query result.
 */
class Unification(val variableValues: VariableBucket = VariableBucket()) {

    fun combinedWith(other: Unification): Unification {
        return Unification(variableValues.combinedWith(other.variableValues))
    }

    fun translate(mapper: (Variable) -> Variable): Unification = Unification(variableValues.translate(mapper))

    override fun toString(): String {
        return variableValues.values.joinToString(", ") { (variable, value) ->
            "$variable = ${value ?: Variable.ANONYMOUS}"
        }
    }

    override fun equals(other: Any?): Boolean {
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
        val TRUE: Unification = Unification()
        val NONE: LazySequence<Unification> = LazySequence.empty()
    }
}