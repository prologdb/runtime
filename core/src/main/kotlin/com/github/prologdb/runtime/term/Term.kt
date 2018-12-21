package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.unification.Unification

interface Term : Comparable<Term> {
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

    /**
     * Like [toString] but instead of going for strict notation (predicates ALWAYS in the syntax
     * `name ( <arguments ... > )`), uses the operators from the given registry. Adds parenthesis
     * in such a way that parsing the resulting string back using the same operators results in 
     * the exact same term.
     * 
     * **Important:** unless for trivial/atomic terms (for which this method likely just delegates
     * to [toString]), **this method is expensive** and should only be called for nice display to humans.
     */
    fun toStringUsingOperatorNotations(operators: OperatorRegistry): String = toString()
}