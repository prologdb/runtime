package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.unification.VariableBucket
import com.github.tmarsteel.ktprolog.unification.VariableDiscrepancyException

class List(givenElements: kotlin.collections.List<Term>, givenTail: Term? = null) : Term {

    val elements: kotlin.collections.List<Term>
    val tail: Variable?

    init {
        if (givenTail is List) {
            elements = givenElements + givenTail.elements
            tail = givenTail.tail
        }
        else {
            elements = givenElements
            tail = givenTail as Variable?
        }
    }


    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Variable) {
            return rhs.unify(this)
        }
        else if (rhs is List) {
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
                    val vars = VariableBucket()
                    vars.instantiate(rhsTail, List(this.elements.subList(index, elements.size), this.tail))
                    try {
                        return carryUnification.combineWith(Unification(vars))
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
                        carryUnification = carryUnification.combineWith(iterationUnification)
                    }
                    catch (ex: VariableDiscrepancyException) {
                        return Unification.FALSE
                    }
                }
            }

            if (this.tail != null || rhs.tail != null) {
                val tailUnification: Unification

                if (this.tail != null && rhs.tail != null) {
                    tailUnification = this.tail.unify(rhs.tail!!, randomVarsScope)
                }
                else if (this.tail != null) {
                    tailUnification = this.tail.unify(List(emptyList()))
                }
                else {
                    tailUnification = rhs.tail!!.unify(List(emptyList()))
                }

                try {
                    carryUnification = carryUnification.combineWith(tailUnification)
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

    override val variables: Set<Variable>
        get() {
            val elements = elements.flatMap(Term::variables).toMutableSet()
            if (tail != null) {
                elements.add(tail)
            }

            return elements
        }

    override fun substituteVariables(mapper: (Variable) -> Term): List {
        return List(elements.map { it.substituteVariables(mapper) }, tail?.substituteVariables(mapper))
    }

    override fun toString(): String {
        var out = "[" + elements.joinToString(",")
        if (tail != null) {
            out += "|$tail"
        }
        return out + "]"
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is List) return false

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