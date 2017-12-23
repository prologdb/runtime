package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.term.Variable

/**
 * A single possible way to unify two expressions; one query result.
 */
class Unification(public val variableValues: VariableBucket = VariableBucket()) {

    fun combineWith(other: Unification): Unification {
        return Unification(variableValues.combineWith(other.variableValues))
    }

    override fun toString(): String {
        return variableValues.values
                .map { (variable, value) -> "$variable = ${value ?: Variable.ANONYMOUS}" }
                .joinToString(", ")
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

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
        val NONE: Sequence<Unification> = emptySet<Unification>().asSequence()
    }
}