package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.UnificationBuilder
import com.github.prologdb.runtime.unification.VariableDiscrepancyException
import com.github.prologdb.runtime.util.OperatorRegistry
import kotlin.math.min

@PrologTypeName("list")
open class PrologList(givenElements: List<Term>, givenTail: Term? = null) : Term {

    val elements: List<Term>
    val tail: Variable?

    init {
        if (givenTail is PrologList) {
            elements = givenElements + givenTail.elements
            tail = givenTail.tail
        } else {
            elements = givenElements
            tail = givenTail as Variable?
        }
    }

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Variable) {
            return rhs.unify(this, randomVarsScope)
        } else if (rhs is PrologList) {
            if (rhs.elements.size > this.elements.size) {
                return rhs.unify(this, randomVarsScope)
            }

            // this' size is always equal to or greater than that of rhs

            if (this.elements.size > rhs.elements.size && rhs.tail == null) {
                return Unification.FALSE
            }

            var carryUnification = UnificationBuilder()
            for (index in 0..this.elements.lastIndex) {
                var iterationUnification: Unification?

                if (index > rhs.elements.lastIndex) {
                    // end of rhs is reached
                    // the rest of the elements in this becomes rhs' tail
                    val rhsTail = rhs.tail!! // should be caught earlier
                    val tailUnification = rhsTail.unify(PrologList(this.elements.subList(index, elements.size), this.tail), randomVarsScope)
                    try {
                        carryUnification.incorporate(tailUnification, randomVarsScope)
                        return carryUnification.build()
                    } catch (ex: VariableDiscrepancyException) {
                        return Unification.FALSE
                    }
                } else {
                    val lhsElement = this.elements[index]
                    val rhsElement = rhs.elements[index]
                    iterationUnification = lhsElement.unify(rhsElement, randomVarsScope)
                }

                if (iterationUnification == null) {
                    return Unification.FALSE
                } else {
                    try {
                        carryUnification.incorporate(iterationUnification, randomVarsScope)
                    } catch (ex: VariableDiscrepancyException) {
                        return Unification.FALSE
                    }
                }
            }

            if (this.tail != null || rhs.tail != null) {
                val tailUnification: Unification

                if (this.tail != null && rhs.tail != null) {
                    tailUnification = this.tail.unify(rhs.tail, randomVarsScope)
                } else if (this.tail != null) {
                    tailUnification = this.tail.unify(PrologList(emptyList()), randomVarsScope)
                } else {
                    tailUnification = rhs.tail!!.unify(PrologList(emptyList()), randomVarsScope)
                }

                try {
                    carryUnification.incorporate(tailUnification, randomVarsScope)
                } catch (ex: VariableDiscrepancyException) {
                    return Unification.FALSE
                }
            }

            return carryUnification.build()
        } else {
            return Unification.FALSE
        }
    }

    override val variables: Set<Variable> by lazy {
        val variables = elements.flatMap(Term::variables).toMutableSet()

        if (tail != null) {
            variables.add(tail)
        }

        variables
    }

    override val isGround: Boolean by lazy { tail == null && elements.all { it.isGround } }

    override fun substituteVariables(mapper: (Variable) -> Term): PrologList {
        return PrologList(elements.map { it.substituteVariables(mapper) }, tail?.substituteVariables(mapper)).also {
            it.sourceInformation = this.sourceInformation
        }
    }

    override fun toString(): String {
        var out = "[" + elements.joinToString(",")
        if (tail != null) {
            out += "|$tail"
        }
        return "$out]"
    }

    override fun toStringUsingOperatorNotations(operators: OperatorRegistry): String {
        var out = "[" + elements.joinToString(
            separator = ", ",
            transform = { it.toStringUsingOperatorNotations(operators) }
        )
        if (tail != null) {
            out += "|${tail.toStringUsingOperatorNotations(operators)}"
        }
        return "$out]"
    }

    override fun compareTo(other: Term): Int {
        if (other is Variable || other is PrologNumber || other is PrologString || other is Atom) {
            // these are by category lesser than compound terms
            return 1
        }

        if (other is PrologList) {
            // arity equal and functor name are equal, sort by elements
            if (this.elements.isEmpty()) {
                return if (other.elements.isEmpty()) 0 else -1
            }

            // in ISO prolog, lists are trees of ./2 and now compared by elements
            // which means that in case of an element tie the tails are compared

            val comparableElementCount = min(this.elements.size, other.elements.size)

            for (i in 0 until comparableElementCount) {
                val elementResult = this.elements[i].compareTo(other.elements[i])
                if (elementResult != 0) {
                    return elementResult
                }
            }

            if (this.tail != null && other.tail != null) {
                return this.tail.compareTo(other.tail)
            }

            // variables are always less than lists
            val thisTailValue = if (this.tail == null) 1 else 0 // no tail => empty list, which is more than given tail (variable)
            val otherTailValue = if (other.tail == null) 1 else 0

            return thisTailValue - otherTailValue
        }

        if (other !is CompoundTerm && other !is PrologDictionary) throw IllegalArgumentException("Given argument is not a known prolog term type (expected variable, number, string, atom, list, compound term or dict)")

        // lists always above compound terms
        return -1
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is PrologList) return false

        if (elements != other.elements) return false
        if (tail != other.tail) return false

        return true
    }

    override fun hashCode(): Int {
        var result = elements.hashCode()
        result = 31 * result + (tail?.hashCode() ?: 0)
        return result
    }

    override var sourceInformation: PrologSourceInformation = NullSourceInformation
}
