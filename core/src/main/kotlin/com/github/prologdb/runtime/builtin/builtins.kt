package com.github.prologdb.runtime.builtin

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

internal val A = Variable("A")
internal val B = Variable("B")
internal val C = Variable("C")
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
typealias PrologBuiltinImplementation = suspend LazySequenceBuilder<Unification>.(Array<out Term>, ProofSearchContext) -> Unit

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
    override val findProofWithin: suspend LazySequenceBuilder<Unification>.(ProofSearchContext, VariableBucket) -> Unit = { _, _  -> }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return this
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return this
    }

    override fun toString() = "__void"
}

fun prologBuiltin(name: String, arity: Int, code: PrologBuiltinImplementation): Rule {
    val predicate = object : Predicate(name, builtinArgumentVariables.sliceArray(0 until arity)) {
        override fun toString() = "$name/$arity"
    }

    val invocationStackFrame = getInvocationStackFrame()
    val stringRepresentation = """$predicate :- __nativeCode("${invocationStackFrame.fileName}:${invocationStackFrame.lineNumber}")"""

    val builtinStackFrame = PrologStackTraceElement(
        predicate,
        invocationStackFrame.prologSourceInformation
    )

    return object : Rule(predicate, voidQuery) {
        override val fulfill: suspend LazySequenceBuilder<Unification>.(Predicate, ProofSearchContext) -> Unit = { other, context ->
            if (predicate.arity == other.arity && predicate.name == other.name) {
                try {
                    code(this, other.arguments, context)
                } catch (ex: PrologException) {
                    ex.addPrologStackFrame(builtinStackFrame)
                    throw ex
                } catch (ex: Throwable) {
                    val newEx = PrologRuntimeException(ex.message ?: "", ex)
                    newEx.addPrologStackFrame(builtinStackFrame)

                    throw newEx
                }
            }
        }

        override fun toString() = stringRepresentation
    }
}