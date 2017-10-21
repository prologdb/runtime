package com.github.tmarsteel.ktprolog.unification

import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable
import java.util.*

class VariableBucket private constructor(
        /**
         * Each known variable gets a key in this map; The value however is not present if the variable
         * has not been instantiated yet.
         */
        private val variableMap: MutableMap<Variable,Optional<Term>>
) {
    constructor() : this(HashMap<Variable, Optional<Term>>())

    private val substitutionMapper: (Variable) -> Term = { variable ->
        if (isDefined(variable) && isInstantiated(variable) && this[variable] != variable) {
            this[variable].substituteVariables(this.asSubstitutionMapper())
        }
        else {
            variable
        }
    }

    fun asSubstitutionMapper(): (Variable) -> Term = substitutionMapper

    operator fun get(v: Variable): Term {
        if (isInstantiated(v)) {
            return variableMap[v]!!.get()
        }
        else {
            throw NameError("Variable $v has not been instantiated yet.")
        }
    }

    fun define(variable: Variable) {
        if (variable in variableMap) {
            throw NameError("Variable $variable is already defined in this bucket.")
        }

        variableMap.put(variable, Optional.empty())
    }

    fun instantiate(variable: Variable, value: Term) {
        if (isInstantiated(variable)) {
            throw NameError("Variable $variable is already instantiated in this bucket.")
        }

        variableMap[variable] = Optional.of(value)
    }

    fun isDefined(variable: Variable): Boolean {
        return variable in variableMap
    }

    fun isInstantiated(variable: Variable): Boolean {
        if (!isDefined(variable)) {
            throw NameError("Variable $variable is not defined in this bucket.")
        }

        return variableMap[variable]!!.isPresent
    }

    fun combineWith(other: VariableBucket): VariableBucket {
        val copy = copy()
        for ((variableName, othersValue) in other.variableMap) {
            if (variableName !in copy.variableMap) {
                copy.variableMap[variableName] = othersValue
            }
            else {
                val thisValue = copy.variableMap[variableName]!!
                if (thisValue.isPresent && othersValue.isPresent) {
                    if (thisValue != othersValue) {
                        // same variable instantiated to different value
                        throw VariableDiscrepancyException("Cannot combine: variable $variableName is instantiated to unequal values: ${thisValue.get()} and ${othersValue.get()}")
                    }
                }
                else if (othersValue.isPresent) {
                    // this does not have an instantiation, but others does => fine
                    copy.variableMap[variableName] = othersValue
                }
                // else: no need to act
            }
        }

        return copy
    }

    fun copy(): VariableBucket {
        val mapCopy = HashMap<Variable,Optional<Term>>()
        mapCopy.putAll(variableMap)
        return VariableBucket(mapCopy)
    }

    /**
     * Removes all variables from this bucket that are not in the given collection
     */
    fun retainAll(variables: Collection<Variable>) {
        variableMap.keys.removeIf { it !in variables }
    }

    fun withVariablesResolvedFrom(mapping: Map<Variable, Variable>): VariableBucket {
        fun resolve(variable: Variable): Variable {
            var pivot = variable
            while (pivot in mapping) {
                pivot = mapping[pivot]!!
            }
            return pivot
        }

        val newBucket = VariableBucket()
        for ((variable, value) in values) {
            val resolved = resolve(variable)
            newBucket.define(resolved)
            newBucket.instantiate(resolved, value.get())
        }

        return newBucket
    }

    val values: Iterable<Pair<Variable,Optional<Term>>>
        get() = variableMap.map { it.key to it.value }
}

class NameError(message: String, cause: Throwable? = null) : RuntimeException(message, cause)