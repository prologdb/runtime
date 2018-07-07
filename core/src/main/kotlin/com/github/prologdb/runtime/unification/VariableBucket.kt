package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

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
        if (variable == Variable.ANONYMOUS) return

        if (isInstantiated(variable)) {
            throw NameError("Variable $variable is already instantiated in this bucket.")
        }

        variableMap[variable] = value
    }

    fun isInstantiated(variable: Variable): Boolean {
        return variableMap[variable] != null
    }

    /**
     * Copies all instantiations from the given variable bucket to this one
     * @throws VariableDiscrepancyException if the same variable is instantiated to different values in `this` and
     *                                      in `variables`
     */
    fun incorporate(variables: VariableBucket) {
        for ((variable, value) in variables.values) {
            if (variable in variableMap) {
                val thisValue = variableMap[variable]
                if (thisValue != null && thisValue != value) {
                    throw VariableDiscrepancyException("Cannot combine: variable $variable is instantiated to unequal values: ${value} and ${thisValue}")
                }
            }
        }

        for ((variable, value) in variables.values) {
            if (value != null) {
                instantiate(variable, value)
            }
        }
    }

    fun combineWith(other: VariableBucket): VariableBucket {
        val copy = copy()
        copy.incorporate(other)

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
        val removedToSubstitute = mutableMapOf<Variable, Term>()

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

    override fun equals(other: Any?): Boolean {
        if (this === other) return true

        other as VariableBucket

        if (variableMap != other.variableMap) return false

        return true
    }

    override fun hashCode(): Int {
        return variableMap.hashCode()
    }

    val values: Iterable<Pair<Variable,Term?>>
        get() = variableMap.map { it.key to it.value }


}

class NameError(message: String, override val cause: Throwable? = null) : RuntimeException(message)