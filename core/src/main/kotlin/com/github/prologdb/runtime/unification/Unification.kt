package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.CircularTermException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import java.util.Collections
import kotlin.math.max
import kotlin.math.min

class UnificationBuilder(private val variableMap: MutableMap<Variable, Term>) {
    constructor() : this(10)
    constructor(initialCapacity: Int) : this(HashMap(initialCapacity))

    private var built = false
    private var corrupted = false

    fun instantiate(variable: Variable, value: Term, randomVariableScope: RandomVariableScope) {
        checkCleanState()

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
        checkCleanState()

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

    fun checkCleanState() {
        check(!built) { "Using UnificationBuilder after build() has been called" }
        checkNotCorrupted()
    }
}

class Unification internal constructor(
    private val variableMap: Map<Variable, Term> = mutableMapOf()
) {
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

    fun isInstantiated(variable: Variable): Boolean {
        return variableMap[variable] != null
    }

    fun copyToBuilder(): UnificationBuilder = UnificationBuilder(HashMap(variableMap))

    /**
     * @param replaceInline if true, [Variable]s in the values from [other] will be substituted with the values
     * from this unification.
     */
    @JvmOverloads
    fun combinedWith(other: Unification, randomVariableScope: RandomVariableScope, replaceInline: Boolean = false): Unification {
        return copyToBuilder().run {
            incorporate(other, randomVariableScope, replaceInline)
            build()
        }
    }

    fun combinedWithCurrentStateOf(other: UnificationBuilder, randomVariableScope: RandomVariableScope): Unification {
        other.checkNotCorrupted()

        return copyToBuilder().run {
            incorporateCurrentStateOf(other, randomVariableScope)
            build()
        }
    }

    /**
     * @return a copy of this [Unification] with only the variables that are also in [retain]
     */
    fun subset(retain: Set<Variable>): Unification {
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
        for ((variable, value) in values) {
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
                if (this in bucket[toSort].variables) {
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

    val values: Iterable<Pair<Variable,Term>>
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