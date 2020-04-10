package com.github.prologdb.runtime.term

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.analyzation.constraint.DeterminismLevel
import com.github.prologdb.runtime.analyzation.constraint.InvocationBehaviour
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.proofsearch.BehaviourExposingPrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorRegistry
import com.github.prologdb.runtime.util.OperatorType
import com.github.prologdb.runtime.util.OperatorType.FX
import com.github.prologdb.runtime.util.OperatorType.FY
import com.github.prologdb.runtime.util.OperatorType.XF
import com.github.prologdb.runtime.util.OperatorType.XFX
import com.github.prologdb.runtime.util.OperatorType.XFY
import com.github.prologdb.runtime.util.OperatorType.YF
import com.github.prologdb.runtime.util.OperatorType.YFX
import sensibleHashCode
import unify

open class CompoundTerm(
        override val functor: String,
        arguments: Array<out Term>
) : Term, Clause, BehaviourExposingPrologCallable
{
    open val arguments: Array<out Term> = arguments

    override val arity = arguments.size

    override val prologTypeName = "compound term"

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is CompoundTerm) {
            if (this.functor != rhs.functor) {
                return Unification.FALSE
            }

            return arguments.unify(rhs.arguments, randomVarsScope)
        }
        else if (rhs is Variable) {
            return rhs.unify(this, randomVarsScope)
        }
        else
        {
            return Unification.FALSE
        }
    }

    override val fulfill: suspend LazySequenceBuilder<Unification>.(Array<out Term>, ProofSearchContext) -> Unit =  { arguments, context ->
        val unification = arguments.unify(arguments, context.randomVariableScope)
        if (unification != null) yield(unification)
    }

    override val variables by lazy {
        arguments.flatMap(Term::variables).toSet()
    }

    override fun substituteVariables(mapper: (Variable) -> Term): CompoundTerm {
        return CompoundTerm(functor, arguments.map { it.substituteVariables(mapper) }.toTypedArray())
    }

    override fun compareTo(other: Term): Int {
        if (other is Variable || other is PrologNumber || other is PrologString || other is Atom || other is PrologList) {
            // these are by category lesser than compound terms
            return 1
        }

        other as? CompoundTerm ?: throw IllegalArgumentException("Given argument is not a known prolog term type (expected variable, number, string, atom, list or compound)")

        val arityCmp = this.arity - other.arity
        if (arityCmp != 0) return arityCmp

        val functorNameCmp = this.functor.compareTo(other.functor)
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
        return functor + "(" + arguments.joinToString(", ") + ")"
    }

    override fun toStringUsingOperatorNotations(operators: OperatorRegistry): String {
        return toStringUsingOperatorNotationsInternal(operators).first
    }

    override fun getBehaviours(inRuntime: PrologRuntimeEnvironment, callingModule: Module, level: DeterminismLevel): List<InvocationBehaviour>? {
        return when(level) {
            DeterminismLevel.DETERMINISTIC -> listOf(InvocationBehaviour.unifiesWith(this))
            DeterminismLevel.SEMI_DETERMINISTIC, DeterminismLevel.NON_DETERMINISTIC -> listOf(
                InvocationBehaviour(
                    CompoundTerm(functor, Array(this.arity) { AnonymousVariable }),
                    emptyMap(),
                    listOf(emptyMap())
                )
            )
            else -> null
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is CompoundTerm) return false

        if (functor != other.functor) return false
        if (arguments contentDeepEquals other.arguments) return true

        return false
    }

    override fun hashCode(): Int {
        var result = functor.hashCode()
        result = 31 * result + arguments.sensibleHashCode()
        return result
    }
}

class CompoundBuilder(private val functor: String) {
    operator fun invoke(vararg arguments: Term) = CompoundTerm(functor, arguments)
}

/**
 * @return first: the result string, second: the precedence of the term, third: the topmost/outmost operator
 *         in the string (or null if none)
 */
private fun Term.toStringUsingOperatorNotationsInternal(operators: OperatorRegistry): Triple<String, Short, OperatorDefinition?> {
    if (this !is CompoundTerm) {
        return Triple(this.toStringUsingOperatorNotations(operators), 0, null)
    }
    
    val operator = operators.getOperatorDefinitionsFor(functor)
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
            // priorities are ambiguous, need to look at associativity
            else if (operator.type == FY && argumentTriple.third?.type in setOf(XF, XFX, XFY)) {
                // associativity removes the ambiguity in this case
                ParenthesisRequirement.NOT_REQUIRED
            } else {
                // priorities are directly inverted to what would be if we simply prepended the operator
                // thus, parenthesis are required.
                ParenthesisRequirement.REQUIRED
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
            // priorities are ambiguous, need to look at associativity
            else if (argumentTriple.third?.type in setOf(FX, XFX, YFX) && operator.type == YF) {
                // associativity removes the ambiguity in this case
                ParenthesisRequirement.NOT_REQUIRED
            } else {
                // priorities are directly inverted to what would be if we simply prepended the operator
                // thus, parenthesis are required.
                ParenthesisRequirement.REQUIRED
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
            val leftTriple  = arguments[0].toStringUsingOperatorNotationsInternal(operators)
            val rightTriple = arguments[1].toStringUsingOperatorNotationsInternal(operators)
            
            val leftParenthesisRequirements = if (operator.precedence > leftTriple.second) {
                ParenthesisRequirement.NOT_REQUIRED
            }
            else {
                ParenthesisRequirement.REQUIRED
            }
            
            val rightParenthesisRequirement = if (operator.precedence > rightTriple.second) {
                ParenthesisRequirement.NOT_REQUIRED
            }
            else {
                ParenthesisRequirement.REQUIRED
            }
            
            val leftString = if (leftParenthesisRequirements == ParenthesisRequirement.REQUIRED) {
                "(${leftTriple.first})"
            } else {
                leftTriple.first
            }
            
            val rightString = if (rightParenthesisRequirement == ParenthesisRequirement.REQUIRED) {
                "(${rightTriple.first})"
            } else {
                rightTriple.first
            }
            
            return Triple(
                "$leftString ${operator.name} $rightString",
                operator.precedence,
                operator
            )
        }
        
        else -> throw RuntimeException("This should not have happened. ${OperatorType::javaClass.name}.isPrefix / .isPostfix / .isInfix are erroneous (exactly one of the three MUST be true)")
    }
}

private enum class ParenthesisRequirement {
    REQUIRED,
    NOT_REQUIRED
}

/**
 * @return compatible with [toStringUsingOperatorNotationsInternal], but second always 0 and third always null.
 */
private fun CompoundTerm.toStringThisUsingStrictNotationArgumentsUsingOperatorNotations(operators: OperatorRegistry): Triple<String, Short, OperatorDefinition?> {
    val argumentStrings = arguments.map {
        val triple = it.toStringUsingOperatorNotationsInternal(operators)
        if (triple.third?.name == ",") "(${triple.first})" else triple.first
    }

    return Triple(
        argumentStrings.joinToString(
            separator = ", ",
            prefix = "$functor(", // TODO functor that needs escaping
            postfix = ")"
        ),
        0,
        null
    )
}

