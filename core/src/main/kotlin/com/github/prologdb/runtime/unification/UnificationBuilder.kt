package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

class UnificationBuilder(private val variableMap: MutableMap<Variable, Term>) {
    constructor() : this(10)
    constructor(initialCapacity: Int) : this(HashMap(initialCapacity))

    private var built = false
    private var corrupted = false

    val isEmpty: Boolean get() = variableMap.isEmpty()

    fun instantiate(variable: Variable, value: Term, randomVariableScope: RandomVariableScope) {
        checkModifiable()

        if (variable == Variable.ANONYMOUS) return

        incorporate(Unification.of(variable, value), randomVariableScope, replaceInline = false)
    }

    /**
     * Copies all instantiations from the given variable bucket to this one
     * @throws VariableDiscrepancyException if the same variable is instantiated to different values in `this` and
     *                                      in `variables`
     */
    @JvmOverloads
    fun incorporate(variables: Unification, randomVariableScope: RandomVariableScope, replaceInline: Boolean= false) {
        incorporate(variables.values.toMap(), randomVariableScope, replaceInline)
    }

    fun incorporateCurrentStateOf(other: UnificationBuilder, randomVariableScope: RandomVariableScope) {
        incorporate(other.variableMap, randomVariableScope, false)
    }

    private fun incorporate(variables: Map<Variable, Term>, randomVariableScope: RandomVariableScope, replaceInline: Boolean) {
        checkModifiable()

        for ((variable, otherValue) in variables.entries) {
            val thisValue = variableMap[variable]
            val otherValueSubstituted = if (replaceInline) {
                otherValue.substituteVariables { this.variableMap[it] ?: it }
            } else {
                otherValue
            }

            if (thisValue == null) {
                variableMap[variable] = otherValueSubstituted
                continue
            }

            val unificationResult = thisValue.unify(otherValueSubstituted, randomVariableScope)
            if (unificationResult == null) {
                corrupted = true
                throw VariableDiscrepancyException("Cannot incorporate: variable $variable is instantiated to non-unify values: $otherValue and $thisValue")
            }

            incorporate(unificationResult, randomVariableScope, replaceInline = false)
        }
    }

    fun build(): Unification {
        checkNotCorrupted()
        built = true
        return Unification(variableMap)
    }

    fun asSubstitutionMapper(): (Variable) -> Term = {
        checkNotCorrupted()
        variableMap[it] ?: it
    }

    fun checkNotCorrupted() {
        check(!corrupted) { "This builder is corrupted and cannot be used for further work." }
    }

    private fun checkModifiable() {
        check(!built) { "Using UnificationBuilder after build() has been called" }
        checkNotCorrupted()
    }
}