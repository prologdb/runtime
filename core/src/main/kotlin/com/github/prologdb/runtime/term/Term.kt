package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification

interface Term {
    /**
     * Unifies this term with the other.
     * @return Information about how to unify or `null` if the two terms cannot be unified.
     */
    fun unify(rhs: Term, randomVarsScope: RandomVariableScope = RandomVariableScope()): Unification?

    val variables: Set<Variable>

    fun substituteVariables(mapper: (Variable) -> Term): Term

    /** The name of the type of this term in prolog language lowercase (e.g. atom, list, ...) */
    val prologTypeName: String

    /**
     * Two terms equal when they are the same Prolog structure. This method builds the identity predicate.
     */
    override fun equals(other: Any?): Boolean
}

