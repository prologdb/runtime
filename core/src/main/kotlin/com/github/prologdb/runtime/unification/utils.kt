@file:JvmName("UnificationUtils")
package com.github.prologdb.runtime.unification

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Variable

/**
 * @return Whether the terms in the two buckets are structurally equivalent (that means `==` semantics, but
 * ignoring variable names).
 */
fun Unification.equalsStructurally(other: Unification, randomVariableScope: RandomVariableScope): Boolean {
    val combined = combinedWith(other, randomVariableScope) ?: return false

    for ((combinedVariable, combinedValue) in combined.entries) {
        if (!this.isInstantiated(combinedVariable) && combinedValue !is Variable) {
            return false
        }
        if (!other.isInstantiated(combinedVariable) && combinedValue !is Variable) {
            return false
        }
    }

    return true
}