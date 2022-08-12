package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.util.OperatorRegistry

@PrologTypeName("term")
interface Term : Comparable<Term> {
    /**
     * Unifies this term with the other.
     * @return Information about how to unify or `null` if the two terms cannot be unified.
     */
    fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification?

    val variables: Set<Variable>

    /**
     * True if there are no [Variable]s in this term
     */
    val isGround: Boolean get() = variables.isEmpty()

    fun substituteVariables(mapper: (Variable) -> Term): Term

    val prologTypeName: String
        get() = javaClass.prologTypeName

    /** From where this term was parsed. Set to [com.github.prologdb.runtime.NullSourceInformation] if unavailable. */
    var sourceInformation: PrologSourceInformation

    /**
     * Two terms equal when they are the same Prolog structure. This method builds the identity predicate.
     */
    override fun equals(other: Any?): Boolean

    /**
     * Like [toString] but instead of going for strict notation (compound terms ALWAYS in the syntax
     * `functor ( <arguments ... > )`), uses the operators from the given registry. Adds parenthesis
     * in such a way that parsing the resulting string back using the same operators results in
     * the exact same term.
     *
     * **Important:** unless for trivial/atomic terms (for which this method likely just delegates
     * to [toString]), **this method is expensive** and should only be called for nice display to humans.
     */
    fun toStringUsingOperatorNotations(operators: OperatorRegistry): String = toString()
}
