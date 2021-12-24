@file:JvmName("TermUtils")

package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import java.util.concurrent.ConcurrentHashMap

/**
 * Unifies the two arrays of terms as if the arguments to predicates with equal functors.
 * @return `Unification.FALSE` if the two arrays haf different lengths
 */
fun Array<out Term>.unify(rhs: Array<out Term>, randomVarsScope: RandomVariableScope): Unification? {
    if (size != rhs.size) {
        return Unification.FALSE
    }

    if (size == 0) {
        return Unification.TRUE
    }

    val vars = VariableBucket()
    for (argIndex in 0..lastIndex) {
        val lhsArg = this[argIndex].substituteVariables(vars.asSubstitutionMapper())
        val rhsArg = rhs[argIndex].substituteVariables(vars.asSubstitutionMapper())
        val argUnification = lhsArg.unify(rhsArg, randomVarsScope)

        if (argUnification == null) {
            // the arguments at place argIndex do not unify => the terms don't unify
            return Unification.FALSE
        }

        for ((variable, value) in argUnification.variableValues.values) {
            if (value != null) {
                // substitute all instantiated variables for simplicity
                val substitutedValue = value.substituteVariables(vars.asSubstitutionMapper())
                if (vars.isInstantiated(variable)) {
                    if (vars[variable] != substitutedValue && vars[variable] != value) {
                        // instantiated to different value => no unification
                        return Unification.FALSE
                    }
                } else {
                    vars.instantiate(variable, substitutedValue)
                }
            }
        }
    }

    return Unification(vars)
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
