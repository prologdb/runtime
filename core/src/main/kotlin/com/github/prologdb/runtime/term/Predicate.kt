package com.github.prologdb.runtime.term

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.library.Clause
import com.github.prologdb.runtime.knowledge.library.OperatorDefinition
import com.github.prologdb.runtime.knowledge.library.OperatorRegistry
import com.github.prologdb.runtime.knowledge.library.OperatorType
import com.github.prologdb.runtime.knowledge.library.OperatorType.*
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

    override fun toStringUsingOperatorNotations(operators: OperatorRegistry): String {
        return toStringUsingOperatorNotationsInternal(operators).first
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

/**
 * @return first: the result string, second: the precedence of the term, third: the topmost/outmost operator
 *         in the string (or null if none)
 */
private fun Term.toStringUsingOperatorNotationsInternal(operators: OperatorRegistry): Triple<String, Short, OperatorDefinition?> {
    if (this !is Predicate) {
        return Triple(this.toStringUsingOperatorNotations(operators), 0, null)
    }
    
    val operator = operators.getOperatorDefinitionsFor(name)
        .firstOrNull { it.type.arity == this.arity }
        ?: return toStringThisUsingStrictNotationArgumentsUsingOperatorNotations(operators)
    
    when {
        operator.type.isPrefix -> {
            val arg = arguments[0]
            val argumentTriple = arg.toStringUsingOperatorNotationsInternal(operators)
            
            val parenthesisRequirement = if (operator.precedence > argumentTriple.second) {
                // priorities are clear, looking at associativity or putting parenthesis not necessary
                ParenthesisRequirement.NOT_REQUIRED
            }
            else if (operator.precedence < argumentTriple.second) {
                // priorities are directly inverted to what would be if we simply prepended the operator
                // thus, parenthesis are required.
                ParenthesisRequirement.REQUIRED
            }
            else {
                // priorities are ambiguous, need to look at associativity
                
                if (operator.type == FY && argumentTriple.third?.type in setOf(XF, XFX, XFY)) {
                    // associativity removes the ambiguity in this case
                    ParenthesisRequirement.NOT_REQUIRED
                } else {
                    // priorities are directly inverted to what would be if we simply prepended the operator
                    // thus, parenthesis are required.
                    ParenthesisRequirement.REQUIRED
                }
            }
            
            return when (parenthesisRequirement) {
                ParenthesisRequirement.NOT_REQUIRED -> Triple(operator.name + " " + argumentTriple.first, operator.precedence, operator)
                ParenthesisRequirement.REQUIRED -> {
                    // for prefix, parenthesis are, to the reader, equal to invocation syntax (compare op(a) op (a)).
                    // for easier reading and parsing, we go with the invocation syntax (no whitespace)
                    return Triple(
                        operator.name + "(" + argumentTriple.first + ")",
                        operator.precedence,
                        operator
                    )
                }
            }
        }
        
        operator.type.isPostfix -> {
            val arg = arguments[0]
            val argumentTriple = arg.toStringUsingOperatorNotationsInternal(operators)

            val parenthesisRequirement = if (operator.precedence > argumentTriple.second) {
                // priorities are clear, looking at associativity or putting parenthesis not necessary
                ParenthesisRequirement.NOT_REQUIRED
            }
            else if (operator.precedence < argumentTriple.second) {
                // priorities are directly inverted to what would be if we simply prepended the operator
                // thus, parenthesis are required.
                ParenthesisRequirement.REQUIRED
            }
            else {
                // priorities are ambiguous, need to look at associativity

                if (argumentTriple.third?.type in setOf(FX, XFX, YFX) && operator.type == YF) {
                    // associativity removes the ambiguity in this case
                    ParenthesisRequirement.NOT_REQUIRED
                } else {
                    // priorities are directly inverted to what would be if we simply prepended the operator
                    // thus, parenthesis are required.
                    ParenthesisRequirement.REQUIRED
                }
            }

            return when (parenthesisRequirement) {
                ParenthesisRequirement.NOT_REQUIRED -> Triple(argumentTriple.first + " " + operator.name, operator.precedence, operator)
                ParenthesisRequirement.REQUIRED -> {
                    return Triple(
                        "(" + argumentTriple.first + ") " + operator.name,
                        operator.precedence,
                        operator
                    )
                }
            }
        }
        
        operator.type.isInfix -> {
            // TODO, this is make-the-test-green-TDD-style-code
            return Triple(
                arguments[0].toStringUsingOperatorNotationsInternal(operators).first + " " + operator.name + " " + arguments[1].toStringUsingOperatorNotationsInternal(operators).first,
                operator.precedence,
                operator
            )
        }
        
        else -> throw RuntimeException("This should not have happened. ${OperatorType::class.qualifiedName}.isPrefix / .isPostfix / .isInfix are erroneous (exactly one of the three MUST be true)")
    }
}

private enum class ParenthesisRequirement {
    REQUIRED,
    NOT_REQUIRED
}

/**
 * @return compatible with [toStringUsingOperatorNotationsInternal], but second always 0 and third always null.
 */
private fun Predicate.toStringThisUsingStrictNotationArgumentsUsingOperatorNotations(operators: OperatorRegistry): Triple<String, Short, OperatorDefinition?> {
    val argumentStrings = arguments.map {
        val triple = it.toStringUsingOperatorNotationsInternal(operators)
        if (triple.third?.name == ",") "(${triple.first})" else triple.first
    }

    return Triple(
        argumentStrings.joinToString(
            separator = ", ",
            prefix = "$name(", // TODO name that needs escaping
            postfix = ")"
        ),
        0,
        null
    )
}