package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.CircularTermException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

class VariableBucket private constructor(
    /**
     * Each known variable gets a key in this map; The value however is not present if the variable
     * has not been instantiated yet.
     */
    private val variableMap: MutableMap<Variable, Term>
) {
    constructor() : this(mutableMapOf<Variable, Term>())

    val isEmpty
        get() = variableMap.isEmpty()

    val variables: Set<Variable> = variableMap.keys

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

        if (variableMap[variable]?.let { it == value } == false) {
            throw VariableDiscrepancyException("Variable $variable is already instantiated in this bucket.")
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
    fun incorporate(variables: VariableBucket, randomVariableScope: RandomVariableScope) {
        if (variables === this) {
            return
        }

        for ((variable, otherValue) in variables.values) {
            val thisValue = variableMap[variable]
            if (thisValue == null) {
                instantiate(variable, otherValue)
                continue
            }

            val unificationResult = thisValue.unify(otherValue, randomVariableScope)
                ?: throw VariableDiscrepancyException("Cannot incorporate: variable $variable is instantiated to non-unify values: $otherValue and $thisValue")

            incorporate(unificationResult.variableValues, randomVariableScope)
        }
    }

    fun combinedWith(other: VariableBucket, randomVariableScope: RandomVariableScope): VariableBucket {
        val copy = copy()
        copy.incorporate(other, randomVariableScope)

        return copy
    }

    fun copy(): VariableBucket {
        val mapCopy = mutableMapOf<Variable,Term>()
        mapCopy.putAll(variableMap)
        return VariableBucket(mapCopy)
    }

    /**
     * Removes all variables from this bucket that are not in the given collection
     */
    fun retainAll(variables: Iterable<Variable>) {
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
            variableMap[key] = value.substituteVariables { variable -> removedToSubstitute[variable] ?: variable }
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
            val resolvedValue = value.substituteVariables(::resolve)
            newBucket.instantiate(resolved, resolvedValue)
        }

        return newBucket
    }

    /**
     * Sorts entries in the given bucket such that, when applied in sequence even references between entries
     * in the bucket are resolved correctly. So e.g. given the bucket `B = 1, A = B` and the subject term `foo(A)`,
     * applying the bucket in original order would yield `foo(B)`. After resorting with this function to `A = B, B = 1`
     * the result would be `foo(1)`.
     *
     * *Example:*
     * ```kotlin
     * var _term = originalTerm
     * originalBucket.sortForSubstitution().forEach {
     *     _term = _term.substituteVariables(it.asSubstitutionMapper())
     * }
     * // _term is now correctly substituted
     * ```
     *
     * @return When successively applied using [Term.substituteVariables] all substitutions, including references, are done.
     *
     * @throws CircularTermException If there are circular references in the bucket.
     */
    fun sortForSubstitution(): List<VariableBucket> {
        val variablesToSort = HashSet(this.variables)
        val bucket = this

        fun Variable.isReferenced(): Boolean {
            for (toSort in variablesToSort) {
                if (toSort === this) continue
                if (this in bucket[toSort].variables) {
                    return true
                }
            }

            return false
        }

        val sorted = ArrayList<VariableBucket>(variablesToSort.size)

        while (variablesToSort.isNotEmpty()) {
            val free = variablesToSort.filterNot { it.isReferenced() }
            if (free.isEmpty()) {
                // there are variables left but none of them are free -> circular dependency!
                throw CircularTermException("Circular dependency in variable instantiations between $variablesToSort")
            }

            val subBucket = VariableBucket()
            for (freeVariable in free) {
                subBucket.instantiate(freeVariable, bucket[freeVariable])
            }
            sorted.add(subBucket)

            variablesToSort.removeAll(free)
        }

        return sorted
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

    val values: Iterable<Pair<Variable,Term>>
        get() = variableMap.map { it.key to it.value }


}

class NameError(message: String, override val cause: Throwable? = null) : RuntimeException(message)
