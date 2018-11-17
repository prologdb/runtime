package com.github.prologdb.runtime.term

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.library.Clause
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import sensibleHashCode

open class Predicate(
    override val name: String,
    arguments: Array<out Term>
) : Term, Clause
{
    open val arguments: Array<out Term> = arguments

    override val arity = arguments.size

    override val prologTypeName = "predicate"

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Predicate) {
            if (this.name != rhs.name) {
                return Unification.FALSE
            }

            if (this.arguments.size != rhs.arguments.size) {
                return Unification.FALSE
            }

            if (arity == 0) {
                return Unification.TRUE
            }

            val vars = VariableBucket()
            for (argIndex in 0..arguments.lastIndex) {
                val lhsArg = arguments[argIndex].substituteVariables(vars.asSubstitutionMapper())
                val rhsArg = rhs.arguments[argIndex].substituteVariables(vars.asSubstitutionMapper())
                val argUnification = lhsArg.unify(rhsArg, randomVarsScope)

                if (argUnification == null) {
                    // the arguments at place argIndex do not unify => the predicates don't unify
                    return Unification.FALSE
                }

                for ((variable, value) in argUnification.variableValues.values) {
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
        else if (rhs is Variable) {
            return rhs.unify(this)
        }
        else
        {
            return Unification.FALSE
        }
    }

    override val unifyWithKnowledge: suspend LazySequenceBuilder<Unification>.(Predicate, ProofSearchContext) -> Unit =  { other, context ->
        val unification = unify(other, context.randomVariableScope)
        if (unification != null) yield(unification)
    }

    override val variables = arguments.flatMap(Term::variables).toSet()

    override fun substituteVariables(mapper: (Variable) -> Term): Predicate {
        return Predicate(name, arguments.map { it.substituteVariables(mapper) }.toTypedArray())
    }

    override fun compareTo(other: Term): Int {
        if (other is Variable || other is PrologNumber || other is PrologString || other is Atom || other is PrologList) {
            // these are by category lesser than compound terms
            return 1
        }

        other as? Predicate ?: throw IllegalArgumentException("Given argument is not a known prolog term type (expected variable, number, string, atom, list or predicate)")

        val arityCmp = this.arity - other.arity
        if (arityCmp != 0) return arityCmp

        val functorNameCmp = this.name.compareTo(other.name)
        if (functorNameCmp != 0) return functorNameCmp

        for (argumentIndex in 0 until arity) {
            val selfArgument = this.arguments[argumentIndex]
            val otherArgument = other.arguments[argumentIndex]

            val argumentCmp = selfArgument.compareTo(otherArgument)
            if (argumentCmp != 0) return argumentCmp
        }

        return 0
    }

    override fun toString(): String {
        return name + "(" + arguments.joinToString(", ") + ")"
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Predicate) return false

        if (name != other.name) return false
        if (arguments contentDeepEquals other.arguments) return true

        return false
    }

    override fun hashCode(): Int {
        var result = name.hashCode()
        result = 31 * result + arguments.sensibleHashCode()
        return result
    }
}

class PredicateBuilder(private val predicateName: String) {
    operator fun invoke(vararg arguments: Term) = Predicate(predicateName, arguments)
}