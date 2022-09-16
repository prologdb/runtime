package com.github.prologdb.runtime.unification

import com.github.prologdb.async.LazySequence
import com.github.prologdb.runtime.CircularTermException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import java.util.Collections
import kotlin.math.max
import kotlin.math.min

interface Unification {
    val isEmpty: Boolean
    val variables: Set<Variable>

    /**
     * @return variables that are instantiated to equal (`==`) values,
     * where there is more than one variable instantiated to that value.
     * The values can be variables as well.
     */
    val commonValues: Map<Term, Set<Variable>>
    val values: Iterable<Pair<Variable, Term>>
    fun asSubstitutionMapper(): (Variable) -> Term
    operator fun get(v: Variable): Term
    fun isInstantiated(variable: Variable): Boolean
    fun combinedWith(other: Unification, randomVariableScope: RandomVariableScope): Unification
    fun createMutableCopy(): MutableUnification
    fun withVariablesResolvedFrom(mapping: VariableMapping): MutableUnification

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
    fun sortedForSubstitution(): List<Unification>

    /**
     * @see Unification.compact
     */
    fun compacted(randomVariableScope: RandomVariableScope): Unification

    /**
     * @return a copy of this [Unification] with only the variables that are also in [retain]
     */
    fun subset(retain: Set<Variable>): Unification

    companion object {
        @JvmStatic
        @get:JvmName("true")
        val TRUE: Unification = UnificationImpl(Collections.unmodifiableMap(mapOf()))

        @JvmStatic
        @get:JvmName("false")
        val FALSE: Unification? = null

        @JvmStatic
        @get:JvmName("none")
        val NONE: LazySequence<Unification> = LazySequence.empty()

        @JvmStatic
        fun whether(condition: Boolean): Unification? = if(condition) TRUE else FALSE

        @JvmStatic
        fun of(variable: Variable, value: Term): Unification = UnificationImpl(HashMap<Variable, Term>(1).apply {
            put(variable, value)
        })

        @JvmStatic
        fun fromMap(map: Map<Variable, Term>): Unification {
            val implMap = HashMap<Variable, Term>(map.size)
            implMap.putAll(map)
            return UnificationImpl(implMap)
        }
    }
}

interface MutableUnification : Unification {

    fun instantiate(variable: Variable, value: Term)

    /**
     * Copies all instantiations from the given variable bucket to this one
     * @throws VariableDiscrepancyException if the same variable is instantiated to different values in `this` and
     *                                      in `variables`
     */
    fun incorporate(variables: Unification, randomVariableScope: RandomVariableScope)

    companion object {
        @JvmName("createEmpty")
        operator fun invoke(): MutableUnification = UnificationImpl()

        @JvmStatic
        fun createTrue(): MutableUnification = UnificationImpl()

        @JvmStatic
        fun createFalse(): MutableUnification? = null

        @JvmStatic
        fun whether(condition: Boolean): MutableUnification? = if(condition) createTrue() else null
    }
}

private class UnificationImpl(
    private val variableMap: MutableMap<Variable, Term> = mutableMapOf()
) : MutableUnification {
    override val isEmpty
        get() = variableMap.isEmpty()

    override val variables: Set<Variable> = variableMap.keys

    private val substitutionMapper: (Variable) -> Term = { variable ->
        if (isInstantiated(variable) && this[variable] != variable) {
            this[variable].substituteVariables(this.asSubstitutionMapper())
        }
        else {
            variable
        }
    }

    override fun asSubstitutionMapper(): (Variable) -> Term = substitutionMapper

    override operator fun get(v: Variable): Term {
        if (isInstantiated(v)) {
            return variableMap[v]!!
        }
        else {
            throw NameError("Variable $v has not been instantiated yet.")
        }
    }

    override fun instantiate(variable: Variable, value: Term) {
        if (variable == Variable.ANONYMOUS) return

        if (variableMap[variable]?.let { it == value } == false) {
            throw VariableDiscrepancyException("Variable $variable is already instantiated in this bucket.")
        }

        variableMap[variable] = value
    }

    override fun isInstantiated(variable: Variable): Boolean {
        return variableMap[variable] != null
    }

    override fun incorporate(variables: Unification, randomVariableScope: RandomVariableScope) {
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

            incorporate(unificationResult, randomVariableScope)
        }
    }

    override fun combinedWith(other: Unification, randomVariableScope: RandomVariableScope): Unification {
        val copy = createMutableCopy(other.variables.size)
        copy.incorporate(other, randomVariableScope)

        return copy
    }

    override fun createMutableCopy(): MutableUnification = createMutableCopy(0)

    private fun createMutableCopy(additionalCapacity: Int = 0): MutableUnification {
        val mapCopy = HashMap<Variable,Term>(variableMap.size + additionalCapacity)
        mapCopy.putAll(variableMap)
        return UnificationImpl(mapCopy)
    }

    override fun subset(retain: Set<Variable>): Unification {
        if (retain.isEmpty() || variableMap.isEmpty()) {
            return Unification.TRUE
        }

        // TODO: can we optimize sensibly for the case variableMap.keys == retain?
        // TODO: can the signature be Iterable<Variable>?

        val newMap = HashMap<Variable, Term>(min(variableMap.size, retain.size))
        val removedToSubstitute = HashMap<Variable, Term>(max(variableMap.size - retain.size, 3))
        for ((variable, value) in variableMap) {
            if (variable in retain) {
                newMap[variable] = value
            } else {
                removedToSubstitute[variable] = value
            }
        }

        for ((key, value) in newMap) {
            newMap[key] = value.substituteVariables { variable -> removedToSubstitute[variable] ?: variable }
        }

        return UnificationImpl(newMap)
    }

    override fun withVariablesResolvedFrom(mapping: VariableMapping): MutableUnification {
        fun resolve(variable: Variable): Variable {
            var pivot = variable
            while (mapping.hasSubstitution(pivot)) {
                pivot = mapping.getOriginal(pivot)!!
            }
            return pivot
        }

        val newBucket = UnificationImpl()
        for ((variable, value) in values) {
            val resolved = resolve(variable)
            val resolvedValue = value.substituteVariables(::resolve)
            newBucket.instantiate(resolved, resolvedValue)
        }

        return newBucket
    }

    override fun sortedForSubstitution(): List<UnificationImpl> {
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

        val sorted = ArrayList<UnificationImpl>(variablesToSort.size)

        while (variablesToSort.isNotEmpty()) {
            val free = variablesToSort.filterNot { it.isReferenced() }
            if (free.isEmpty()) {
                // there are variables left but none of them are free -> circular dependency!
                throw CircularTermException("Circular dependency in variable instantiations between $variablesToSort")
            }

            val subBucket = UnificationImpl()
            for (freeVariable in free) {
                subBucket.instantiate(freeVariable, bucket[freeVariable])
            }
            sorted.add(subBucket)

            variablesToSort.removeAll(free)
        }

        return sorted
    }

    override fun compacted(randomVariableScope: RandomVariableScope): Unification {
        if (isEmpty) {
            return this
        }

        val sorted = try {
            sortedForSubstitution()
        }
        catch (ex: CircularTermException) {
            return this
        }

        val result = sorted.first()
        if (sorted.size > 1) {
            for (next in sorted.subList(1, sorted.size)) {
                for (resultVar in result.variables) {
                    result.variableMap[resultVar] = result[resultVar].substituteVariables(next.asSubstitutionMapper())
                }

                result.incorporate(next, randomVariableScope)
            }
        }

        for ((commonValue, variables) in result.commonValues) {
            check(commonValue !in variables)

            var firstVariable: Variable? = null
            if (commonValue is Variable) {
                var secondVariable: Variable? = null
                for (variable in variables) {
                    if (firstVariable == null) {
                        result.variableMap[variable] = commonValue
                        firstVariable = variable
                    } else if (secondVariable == null) {
                        result.variableMap[firstVariable] = variable
                        result.variableMap.remove(variable)
                        secondVariable = variable
                    } else {
                        result.variableMap[variable] = secondVariable
                    }
                }
            } else {
                for (variable in variables) {
                    if (firstVariable == null) {
                        result.variableMap[variable] = commonValue
                        firstVariable = variable
                    } else {
                        result.variableMap[variable] = firstVariable
                    }
                }
            }
        }

        return result
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is UnificationImpl) return false

        if (variableMap != other.variableMap) return false

        return true
    }

    override fun hashCode(): Int {
        return variableMap.hashCode()
    }

    override val commonValues: Map<Term, Set<Variable>> get() {
        return values
            .groupBy { it.second }
            .entries
            .asSequence()
            .filter { it.value.size > 1 }
            .associateTo(HashMap()) { (commonValue, entries) -> commonValue to entries.map { (variable, _) -> variable }.toSet() }
    }

    override val values: Iterable<Pair<Variable,Term>>
        get() = variableMap.map { it.key to it.value }
}

