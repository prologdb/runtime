package com.github.tmarsteel.ktprolog

import com.github.tmarsteel.ktprolog.term.Variable

/**
 * Used when variables in a term are substituted to document the changes so that, later, the change
 * can be reverted with different values for the original variables.
 */
class VariableMapping {
    private val aToB: MutableMap<Variable, Variable> = mutableMapOf()
    private val bToA: MutableMap<Variable, Variable> = mutableMapOf()

    fun storeSubstitution(original: Variable, substitution: Variable) {
        if (aToB.containsKey(original)) {
            throw IllegalStateException()
        }

        aToB[original] = substitution
        bToA[substitution] = original
    }

    fun hasOriginal(original: Variable): Boolean = original in aToB
    fun hasSubstitution(substitution: Variable): Boolean = substitution in bToA

    fun getSubstitution(original: Variable): Variable? = aToB[original]
    fun getOriginal(substitution: Variable): Variable? = bToA[substitution]
}