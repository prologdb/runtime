package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.knowledge.RandomVariableScope
import com.github.tmarsteel.ktprolog.unification.Unification

interface Term {
    /**
     * Unifies this term with the other.
     * @return Information about how to unify or `null` if the two terms cannot be unified.
     */
    fun unify(rhs: Term, randomVarsScope: RandomVariableScope = RandomVariableScope()): Unification?

    val variables: Set<Variable>

    fun substituteVariables(mapper: (Variable) -> Term): Term
}

