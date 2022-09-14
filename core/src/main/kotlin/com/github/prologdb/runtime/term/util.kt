@file:JvmName("TermUtils")
package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.MutableUnification
import java.util.concurrent.ConcurrentHashMap

/**
 * Unifies the two arrays of terms as if the arguments to predicates with equal functors.
 * @return `Unification.FALSE` if the two arrays haf different lengths
 */
fun Array<out Term>.unify(rhs: Array<out Term>, randomVarsScope: RandomVariableScope): MutableUnification? {
    if (size != rhs.size) {
        return MutableUnification.createFalse()
    }

    if (size == 0) {
        return MutableUnification.createTrue()
    }

    val vars = MutableUnification()
    for (argIndex in 0..lastIndex) {
        val lhsArg = this[argIndex].substituteVariables(vars.asSubstitutionMapper())
        val rhsArg = rhs[argIndex].substituteVariables(vars.asSubstitutionMapper())
        val argUnification = lhsArg.unify(rhsArg, randomVarsScope)

        if (argUnification == null) {
            // the arguments at place argIndex do not unify => the terms don't unify
            return MutableUnification.createFalse()
        }

        for ((variable, value) in argUnification.variableValues.values) {
            // substitute all instantiated variables for simplicity
            val substitutedValue = value.substituteVariables(vars.asSubstitutionMapper())
            if (vars.isInstantiated(variable)) {
                if (vars[variable] != substitutedValue && vars[variable] != value) {
                    // instantiated to different value => no unification
                    return MutableUnification.createFalse()
                }
            } else {
                vars.instantiate(variable, substitutedValue)
            }
        }
    }

    return vars
}

val Array<out Term>.variables: Iterable<Variable>
    get() = object : Iterable<Variable> {
        override fun iterator() = object : Iterator<Variable> {
            private var currentIndex = 0
            private var currentSub: Iterator<Variable>? = null

            override fun hasNext(): Boolean {
                if (currentIndex >= this@variables.size) {
                    return false
                }

                if (currentSub == null) {
                    currentSub = this@variables[currentIndex].variables.iterator()
                }

                if (!currentSub!!.hasNext()) {
                    currentIndex++
                    currentSub = null
                    return hasNext()
                }

                return true
            }

            override fun next(): Variable {
                if (!hasNext()) {
                    throw NoSuchElementException()
                }

                return (currentSub ?: throw NoSuchElementException()).next()
            }
        }
    }

private val prologTypeNameCache = ConcurrentHashMap<Class<out Term>, String>()
val Class<out Term>.prologTypeName: String
    get() = prologTypeNameCache.computeIfAbsent(this) { clazz ->
        clazz.getAnnotation(PrologTypeName::class.java)?.value
            ?: clazz.simpleName
    }

/**
 * Implements structural equality (`=@=/2`).
 * @return whether the two terms are equal (`==/2` semantics), but ignoring variable names.
 */
fun Term.equalsStructurally(other: Term, randomVarsScope: RandomVariableScope): Boolean {
    val unification = this.unify(other, randomVarsScope) ?: return false
    return unification.values.all { (_, instantiatedTo) -> instantiatedTo is Variable }
}

/**
 * Replaces all variables in the term by instances of `$VAR(N)`, where N is an integer
 * corresponding to the index of the variable in the term. Multiple mentions of the same
 * variable will get different indexes. So e.g. `foo(a, X, X, Y)` will turn into
 * `foo(a, $VAR(0), $VAR(1), $VAR(2))`
 */
fun Term.numberVariables(): Term {
    var variableCounter = 0L
    return substituteVariables { CompoundTerm("\$VAR", arrayOf(PrologNumber(variableCounter++))) }
}

/**
 * If this term is an integer [PrologNumber] and in the given [range], returns this
 * number as a [Long]; null otherwise.
 */
fun Term.asIntegerInRange(range: LongRange): Long? {
    if (this !is PrologNumber) {
        return null
    }

    if (!this.isInteger) {
        return null
    }

    val longValue = try {
        toInteger()
    } catch (_: ArithmeticException) {
        return null
    }

    return longValue.takeIf { it in range }
}