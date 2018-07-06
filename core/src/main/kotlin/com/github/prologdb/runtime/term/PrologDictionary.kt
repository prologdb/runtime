package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification

class PrologDictionary(val tag: Term?, val pairs: Map<String, Term>) : Term {
    init {
        if (tag !is Atom? && tag !is Variable?) {
            throw IllegalArgumentException("The tag must be an atom or a variable")
        }
    }

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Variable) return rhs.unify(this)

        if (rhs !is PrologDictionary) return Unification.FALSE

        if (this.pairs.size != rhs.pairs.size) {
            return Unification.FALSE
        }

        if ((this.tag != null && rhs.tag == null) || (this.tag == null && rhs.tag != null)) {
            // tags cannot unify
            return Unification.FALSE
        }

        val tagUnification = if (this.tag == null) Unification.TRUE else this.tag.unify(rhs.tag!!)
        if (tagUnification == null) {
            return Unification.FALSE
        }

        if (pairs.isEmpty()) {
            return tagUnification
        }

        val vars = tagUnification.variableValues
        for (pair in pairs) {
            val rhsValue = rhs.pairs[pair.key]?.substituteVariables(vars.asSubstitutionMapper())
            if (rhsValue == null) {
                // rhs has no value for this key, cannot possibly unify
                return Unification.FALSE
            }

            val lhsValue = pair.value.substituteVariables(vars.asSubstitutionMapper())
            val pairUnification = lhsValue.unify(rhsValue, randomVarsScope)

            if (pairUnification == null) {
                // the values for the keys do not unify => the dicts don't unify
                return Unification.FALSE
            }

            for ((variable, value) in pairUnification.variableValues.values) {
                if (value != null) {
                    // substitute all instantiated variables for simplicity and performance
                    val substitutedValue = value.substituteVariables(vars.asSubstitutionMapper())
                    if (vars.isInstantiated(variable)) {
                        if (vars[variable] != substitutedValue && vars[variable] != value) {
                            // instantiated to different value => no unification
                            return Unification.FALSE
                        }
                    }
                    else {
                        vars.instantiate(variable, substitutedValue)
                    }
                }
            }
        }

        // we made it through all arguments without issues => great
        return Unification(vars)
    }

    override val variables: Set<Variable>
        get() {
            var variables = pairs.values.flatMap { it.variables }
            if (tag != null && tag is Variable) {
                if (variables !is MutableList) variables = variables.toMutableList()
                variables.add(tag)
            }

            return variables.toSet()
        }

    override fun substituteVariables(mapper: (Variable) -> Term): Term
        = PrologDictionary(tag, pairs.mapValues { it.value.substituteVariables(mapper) })

    override val prologTypeName = "dict"

    override fun compareTo(other: Term): Int {
        if (other is Variable || other is PrologNumber || other is PrologString || other is Atom || other is PrologList) {
            // these are by category lesser than compound terms
            return 1
        }

        // dicts are not compared by content, neither against predicates nor against other dicts
        return 0
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as PrologDictionary

        if (pairs != other.pairs) return false

        return true
    }

    override fun hashCode(): Int {
        return pairs.hashCode()
    }
}