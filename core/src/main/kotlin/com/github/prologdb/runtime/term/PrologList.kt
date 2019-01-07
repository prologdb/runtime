package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableDiscrepancyException

open class PrologList(givenElements: kotlin.collections.List<Term>, givenTail: Term? = null) : Term {

    open val elements: kotlin.collections.List<Term>
    val tail: Variable?

    init {
        if (givenTail is PrologList) {
            elements = givenElements + givenTail.elements
            tail = givenTail.tail
        }
        else {
            elements = givenElements
            tail = givenTail as Variable?
        }
    }

    override val prologTypeName = "list"

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Variable) {
            return rhs.unify(this)
        }
        else if (rhs is PrologList) {
            if (rhs.elements.size > this.elements.size) {
                return rhs.unify(this, randomVarsScope)
            }

            // this' size is always equal to or greater than that of rhs

            if (this.elements.size > rhs.elements.size && rhs.tail == null) {
                return Unification.FALSE
            }

            var carryUnification = Unification()
            for (index in 0..this.elements.lastIndex) {
                var iterationUnification: Unification?

                if (index > rhs.elements.lastIndex) {
                    // end of rhs is reached
                    // the rest of the elements in this becomes rhs' tail
                    val rhsTail = rhs.tail!! // should be caught earlier
                    try {
                        return carryUnification.combinedWith(rhsTail.unify(PrologList(this.elements.subList(index, elements.size), this.tail)))
                    }
                    catch (ex: VariableDiscrepancyException) {
                        return Unification.FALSE
                    }
                }
                else {
                    val lhsElement = this.elements[index]
                    val rhsElement = rhs.elements[index]
                    iterationUnification = lhsElement.unify(rhsElement, randomVarsScope)
                }

                if (iterationUnification == null) {
                    return Unification.FALSE
                }
                else {
                    try {
                        carryUnification = carryUnification.combinedWith(iterationUnification)
                    }
                    catch (ex: VariableDiscrepancyException) {
                        return Unification.FALSE
                    }
                }
            }

            if (this.tail != null || rhs.tail != null) {
                val tailUnification: Unification

                if (this.tail != null && rhs.tail != null) {
                    tailUnification = this.tail.unify(rhs.tail, randomVarsScope)
                }
                else if (this.tail != null) {
                    tailUnification = this.tail.unify(PrologList(emptyList()))
                }
                else {
                    tailUnification = rhs.tail!!.unify(PrologList(emptyList()))
                }

                try {
                    carryUnification = carryUnification.combinedWith(tailUnification)
                }
                catch (ex: VariableDiscrepancyException) {
                    return Unification.FALSE
                }
            }

            return carryUnification
        }
        else {
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

    override fun substituteVariables(mapper: (Variable) -> Term): PrologList {
        return PrologList(elements.map { it.substituteVariables(mapper) }, tail?.substituteVariables(mapper))
    }

    override fun toString(): String {
        var out = "[" + elements.joinToString(",")
        if (tail != null) {
            out += "|$tail"
        }
        return out + "]"
    }

    override fun toStringUsingOperatorNotations(operators: OperatorRegistry): String {
        var out = "[" + elements.joinToString(
            separator = ", ",
            transform = { it.toStringUsingOperatorNotations(operators) }
        )
        if (tail != null) {
            out += "|${tail.toStringUsingOperatorNotations(operators)}"
        }
        return out + "]"
    }

    override fun compareTo(other: Term): Int {
        if (other is Variable || other is PrologNumber || other is PrologString || other is Atom) {
            // these are by category lesser than compound terms
            return 1
        }

        if (other is PrologList) {
            // arity equal and functor name are equal, sort by elements
            if (this.elements.isEmpty() && other.elements.isEmpty()) {
                return 0
            }

            // empty lists are lesser than non-empty lists
            if (this.elements.isEmpty() && other.elements.isNotEmpty()) {
                return -1
            }
            else if (this.elements.isNotEmpty() && other.elements.isEmpty()) {
                return 1
            }

            // both lists have at least one element at this point
            return this.elements[0].compareTo(other.elements[1])
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
}