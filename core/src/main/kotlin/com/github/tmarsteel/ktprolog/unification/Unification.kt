package com.github.tmarsteel.ktprolog.unification

import com.github.tmarsteel.ktprolog.term.Variable

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

    companion object {
        val FALSE: Unification? = null
        val TRUE: Unification = Unification()
        val NONE: Sequence<Unification> = emptySet<Unification>().asSequence()
    }
}