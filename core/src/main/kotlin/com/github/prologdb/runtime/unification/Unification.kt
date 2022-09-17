package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.CircularTermException
import com.github.prologdb.runtime.PrologInternalError
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import java.util.Collections
import kotlin.math.max
import kotlin.math.min

class Unification internal constructor(
    private val variableMap: Map<Variable, Term> = mutableMapOf()
) {
    val isEmpty
        get() = variableMap.isEmpty()

    val variables: Set<Variable> = variableMap.keys

    fun asSubstitutionMapper(): (Variable) -> Term = this::resolveValue

    operator fun get(v: Variable): Term = variableMap[v] ?: v

    fun isInstantiated(variable: Variable): Boolean = resolveValue(variable) !is Variable

    private tailrec fun resolveValue(v: Variable): Term {
        val value = variableMap[v] ?: return v
        if (value !is Variable) {
            return value
        }

        return resolveValue(value)
    }

    fun toBuilder(): UnificationBuilder = UnificationBuilder(HashMap(variableMap))

    /**
     * @param replaceInline if true, [Variable]s in the values from [other] will be substituted with the values
     * from this unification.
     */
    @JvmOverloads
    fun combinedWith(other: Unification, randomVariableScope: RandomVariableScope, replaceInline: Boolean = false): Unification? {
        if (other.isEmpty) {
            return this
        }

        if (this.isEmpty) {
            return other
        }

        val builder = toBuilder()
        try {
            builder.incorporate(other, randomVariableScope, replaceInline)
        }
        catch (ex: VariableDiscrepancyException) {
            return null
        }

        return builder.build()
    }

    @JvmOverloads
    fun combinedWithExpectSuccess(other: Unification, randomVariableScope: RandomVariableScope, replaceInline: Boolean = false): Unification {
        if (other.isEmpty) {
            return this
        }

        if (this.isEmpty) {
            return other
        }

        val builder = toBuilder()
        try {
            builder.incorporate(other, randomVariableScope, replaceInline)
        }
        catch (ex: VariableDiscrepancyException) {
            throw PrologInternalError("Combination of two unifications should have succeeded but didn't: ${ex.message}", ex)
        }

        return builder.build()
    }

    fun combinedWithCurrentStateOf(other: UnificationBuilder, randomVariableScope: RandomVariableScope): Unification? {
        other.checkNotCorrupted()

        if (other.isEmpty) {
            return this
        }

        val builder = toBuilder()
        try {
            builder.incorporateCurrentStateOf(other, randomVariableScope)
        }
        catch (ex: VariableDiscrepancyException) {
            return null
        }

        return builder.build()
    }

    fun combinedWithCurrentStateOfExpectSuccess(other: UnificationBuilder, randomVariableScope: RandomVariableScope): Unification {
        other.checkNotCorrupted()

        if (other.isEmpty) {
            return this
        }

        val builder = toBuilder()
        try {
            builder.incorporateCurrentStateOf(other, randomVariableScope)
        }
        catch (ex: VariableDiscrepancyException) {
            throw PrologInternalError("Combination of two unifications should have succeeded but didn't: ${ex.message}", ex)
        }

        return builder.build()
    }

    /**
     * @return a copy of this [Unification] with only the variables that are also in [retain]
     */
    fun subset(retain: Set<Variable>): Unification {
        if (retain.isEmpty() || variableMap.isEmpty()) {
            return Unification.TRUE
        }

        val newMap = HashMap<Variable, Term>(min(variableMap.size, retain.size))
        val removedToSubstitute = HashMap<Variable, Term>(max(variableMap.size - retain.size, 3))
        for ((variable, value) in variableMap) {
            if (variable in retain) {
                newMap[variable] = value
            } else {
                removedToSubstitute[variable] = value
            }
        }

        if (removedToSubstitute.isEmpty()) {
            return this
        }

        for ((key, value) in newMap) {
            newMap[key] = value.substituteVariables { variable -> removedToSubstitute[variable] ?: variable }
        }

        return Unification(newMap)
    }

    fun withVariablesResolvedFrom(mapping: VariableMapping, randomVariableScope: RandomVariableScope): Unification {
        fun resolve(variable: Variable): Variable {
            var pivot = variable
            while (mapping.hasSubstitution(pivot)) {
                pivot = mapping.getOriginal(pivot)!!
            }
            return pivot
        }

        val newBucket = UnificationBuilder(variableMap.size)
        for ((variable, value) in entries) {
            val resolved = resolve(variable)
            val resolvedValue = value.substituteVariables(::resolve)
            newBucket.instantiate(resolved, resolvedValue, randomVariableScope)
        }

        return newBucket.build()
    }

    fun sortedForSubstitution(): List<Unification> {
        val variablesToSort = HashSet(this.variables)
        val bucket = this

        fun Variable.isReferenced(): Boolean {
            for (toSort in variablesToSort) {
                if (toSort === this) continue
                if (this in bucket.variableMap[toSort]!!.variables) {
                    return true
                }
            }

            return false
        }

        val sorted = ArrayList<Unification>(variablesToSort.size)

        while (variablesToSort.isNotEmpty()) {
            val free = variablesToSort.filterNot { it.isReferenced() }
            if (free.isEmpty()) {
                // there are variables left but none of them are free -> circular dependency!
                throw CircularTermException("Circular dependency in variable instantiations between $variablesToSort")
            }

            val subVariables = HashMap<Variable, Term>()
            for (freeVariable in free) {
                subVariables[freeVariable] = bucket[freeVariable]
            }
            sorted.add(Unification(subVariables))
            variablesToSort.removeAll(free)
        }

        return sorted
    }

    @JvmOverloads
    fun compacted(aggressive: Boolean = false): Unification {
        if (isEmpty) {
            return this
        }

        val sorted = try {
            sortedForSubstitution()
        }
        catch (ex: CircularTermException) {
            return this
        }

        val result = HashMap(sorted.first().variableMap)
        if (sorted.size > 1) {
            for (next in sorted.subList(1, sorted.size)) {
                for (resultVar in result.keys) {
                    result[resultVar] = result[resultVar]!!.substituteVariables(next.asSubstitutionMapper())
                }

                result.putAll(next.variableMap)
            }
        }

        if (aggressive) {
            for ((commonValue, variables) in result.commonValues) {
                check(commonValue !in variables)

                var firstVariable: Variable? = null
                if (commonValue is Variable) {
                    var secondVariable: Variable? = null
                    for (variable in variables) {
                        if (firstVariable == null) {
                            result[variable] = commonValue
                            firstVariable = variable
                        } else if (secondVariable == null) {
                            result[firstVariable] = variable
                            result.remove(variable)
                            secondVariable = variable
                        } else {
                            result[variable] = secondVariable
                        }
                    }
                } else {
                    for (variable in variables) {
                        if (firstVariable == null) {
                            result[variable] = commonValue
                            firstVariable = variable
                        } else {
                            result[variable] = firstVariable
                        }
                    }
                }
            }
        }

        return Unification(result)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Unification) return false

        if (variableMap != other.variableMap) return false

        return true
    }

    override fun hashCode(): Int {
        return variableMap.hashCode()
    }

    val entries: Iterable<Pair<Variable,Term>>
        get() = variableMap.map { it.key to it.value }

    companion object {
        @JvmStatic
        @get:JvmName("true")
        val TRUE: Unification = Unification(Collections.unmodifiableMap(mapOf()))

        @JvmStatic
        @get:JvmName("false")
        val FALSE: Unification? = null

        @JvmStatic
        fun whether(condition: Boolean): Unification? = if(condition) TRUE else FALSE

        @JvmStatic
        fun of(variable: Variable, value: Term): Unification = Unification(HashMap<Variable, Term>(1).apply {
            put(variable, value)
        })

        @JvmStatic
        fun fromMap(map: Map<Variable, Term>): Unification {
            return Unification(HashMap(map))
        }
    }
}

/**
 * @return variables that are instantiated to equal (`==`) values,
 * where there is more than one variable instantiated to that value.
 * The values can be variables as well.
 */
private val <K, V> Map<K, V>.commonValues: Map<V, Set<K>> get() {
    return entries
        .groupBy { it.value }
        .entries
        .asSequence()
        .filter { it.value.size > 1 }
        .associateTo(HashMap()) { (commonValue, entries) -> commonValue to entries.map { (variable, _) -> variable }.toSet() }
}