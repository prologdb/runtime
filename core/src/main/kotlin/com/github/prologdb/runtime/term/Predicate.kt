package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.library.LibraryEntry
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import sensibleHashCode

open class Predicate(override val name: String, arguments: Array<out Term>) : Term, LibraryEntry
{
    open val arguments: Array<out Term> = arguments

    override val arity = arguments.size

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Predicate) {
            if (this.name != rhs.name) {
                return Unification.FALSE
            }

            if (this.arguments.size != rhs.arguments.size) {
                return Unification.FALSE
            }

            if (arguments.isEmpty()) {
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

    override fun unifyWithKnowledge(other: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        val unification = unify(other, randomVariableScope)
        return if (unification == null) Unification.NONE else LazySequence.of(unification)
    }

    override val variables = arguments.flatMap(Term::variables).toSet()

    override fun substituteVariables(mapper: (Variable) -> Term): Predicate {
        return Predicate(name, arguments.map { it.substituteVariables(mapper) }.toTypedArray())
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