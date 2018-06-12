package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

internal val surrogateVarLHS = Variable("LHS")
internal val surrogateVarRHS = Variable("RHS")
internal val A = Variable("A")
internal val B = Variable("B")
internal val X = Variable("X")

@Deprecated("use the DSL instead")
abstract class BuiltinPredicate(name: String, vararg arguments: Term) : Predicate(name, arguments) {
    override val variables: Set<Variable>
        get() = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this
}


/**
 * Provides the implementation to a builtin. Is intended to be used **ONLY** in
 * combination with [prologBuiltin] to help ensure all preconditions for [invoke].
 *
 * Is invoked when the builtin is invoked from prolog. When invoked, it must be assured that:
 * * the predicate invoked from the prolog code actually matches the builtin (name & arity)
 *
 * Arguments to the function:
 * 1. The arguments given to the builtin from the prolog code that invokes it
 * 2. The knowledge base within which the builtin is being executed
 * 3. Source of random variables to prevent collisions
 */
typealias PrologBuiltinImplementation  = (Array<out Term>, KnowledgeBase, RandomVariableScope) -> LazySequence<Unification>

/**
 * [Variable]s to be used in the "prolog"-ish representation of builtins. E.g.
 * when defining the builtin `string_chars(A, B)`, `A` and `B` are obtained from
 * this list.
 */
private val builtinArgumentVariables = arrayOf(
    Variable("_Arg0"),
    Variable("_Arg1"),
    Variable("_Arg2"),
    Variable("_Arg3"),
    Variable("_Arg4"),
    Variable("_Arg5"),
    Variable("_Arg6"),
    Variable("-Arg7"),
    Variable("_Arg8"),
    Variable("_Arg9")
)

/**
 * This query never has any results. It is used as a placeholder for builtins where
 * an instance of [Query] is required but the actual query never gets invoked because
 * kotlin code implements the builtin.
 */
private val voidQuery = object : Query {
    override fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket, randomVarsScope: RandomVariableScope): LazySequence<Unification> {
        return Unification.NONE
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return this
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return this
    }

    override fun toString() = "__void"
}

fun prologBuiltin(name: String, arity: Int, code: PrologBuiltinImplementation): Rule {
    val predicate = Predicate(name, builtinArgumentVariables.sliceArray(0 until arity))
    return object : Rule(predicate, voidQuery) {
        override fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
            if (predicate.arity != this.arity) return Unification.NONE
            if (predicate.name != this.name) return Unification.NONE

            return code.invoke(predicate.arguments, kb, randomVariableScope)
        }

        override fun toString() = "$predicate :- __nativeCode"
    }
}