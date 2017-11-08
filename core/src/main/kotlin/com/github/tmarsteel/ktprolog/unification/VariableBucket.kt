package com.github.tmarsteel.ktprolog.unification

import com.github.tmarsteel.ktprolog.VariableMapping
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable

class VariableBucket private constructor(
        /**
         * Each known variable gets a key in this map; The value however is not present if the variable
         * has not been instantiated yet.
         */
        private val variableMap: MutableMap<Variable, Term?>
) {
    constructor() : this(mutableMapOf<Variable, Term?>())

    val isEmpty
        get() = variableMap.isEmpty()

    private val substitutionMapper: (Variable) -> Term = { variable ->
        if (isInstantiated(variable) && this[variable] != variable) {
            this[variable].substituteVariables(this.asSubstitutionMapper())
        }
        else {
            variable
        }
    }

    fun asSubstitutionMapper(): (Variable) -> Term = substitutionMapper

    operator fun get(v: Variable): Term {
        if (isInstantiated(v)) {
            return variableMap[v]!!
        }
        else {
            throw NameError("Variable $v has not been instantiated yet.")
        }
    }

    fun instantiate(variable: Variable, value: Term) {
        if (isInstantiated(variable)) {
            throw NameError("Variable $variable is already instantiated in this bucket.")
        }

        variableMap[variable] = value
    }

    fun isInstantiated(variable: Variable): Boolean {
        return variableMap[variable] != null
    }

    fun combineWith(other: VariableBucket): VariableBucket {
        val copy = copy()
        for ((variableName, othersValue) in other.variableMap) {
            if (variableName !in copy.variableMap) {
                copy.variableMap[variableName] = othersValue
            }
            else {
                val thisValue = copy.variableMap[variableName]
                if (thisValue != null && othersValue != null) {
                    if (thisValue != othersValue) {
                        // same variable instantiated to different value
                        throw VariableDiscrepancyException("Cannot combine: variable $variableName is instantiated to unequal values: ${thisValue} and ${othersValue}")
                    }
                }
                else if (othersValue != null) {
                    // this does not have an instantiation, but others does => fine
                    copy.variableMap[variableName] = othersValue
                }
                // else: no need to act
            }
        }

        return copy
    }

    fun copy(): VariableBucket {
        val mapCopy = mutableMapOf<Variable,Term?>()
        mapCopy.putAll(variableMap)
        return VariableBucket(mapCopy)
    }

    /**
     * Removes all variables from this bucket that are not in the given collection
     */
    fun retainAll(variables: Collection<Variable>) {
        val keysToRemove = variableMap.keys.filter { it !in variables }
        val removedToSubstitute = mutableMapOf<Variable,Term>()

        for (key in keysToRemove) {
            val value = variableMap[key]
            if (value != null) {
                removedToSubstitute[key] = value
            }

            variableMap.remove(key)
        }

        for ((key, value) in variableMap) {
            if (value != null) {
                variableMap[key] = value.substituteVariables({ variable -> removedToSubstitute[variable] ?: variable })
            }
        }
    }

    fun withVariablesResolvedFrom(mapping: VariableMapping): VariableBucket {
        fun resolve(variable: Variable): Variable {
            var pivot = variable
            while (mapping.hasSubstitution(pivot)) {
                pivot = mapping.getOriginal(pivot)!!
            }
            return pivot
        }

        val newBucket = VariableBucket()
        for ((variable, value) in values) {
            val resolved = resolve(variable)
            if (value != null) {
                val resolvedValue = value.substituteVariables(::resolve)
                newBucket.instantiate(resolved, resolvedValue)
            }
        }

        return newBucket
    }

    val values: Iterable<Pair<Variable,Term?>>
        get() = variableMap.map { it.key to it.value }
}

class NameError(message: String, override val cause: Throwable? = null) : RuntimeException(message)