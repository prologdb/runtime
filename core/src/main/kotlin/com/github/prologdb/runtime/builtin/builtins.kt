package com.github.prologdb.runtime.builtin

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.PrologStackTraceElement
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.Rule
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.unification.Unification

internal val A = Variable("A")
internal val B = Variable("B")
internal val C = Variable("C")
internal val X = Variable("X")

@Deprecated("use the DSL instead")
abstract class BuiltinPredicate(name: String, vararg arguments: Term) : CompoundTerm(name, arguments) {
    override val variables: Set<Variable>
        get() = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this
}


/**
 * Provides the implementation to a builtin. Is intended to be used **ONLY** in
 * combination with [nativeRule] to help ensure all preconditions for [invoke].
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
 * This query is used as a placeholder for builtins where an instance of [Query] is required
 * but the actual query never gets invoked because kotlin code implements the builtin.
 */
private val nativeCodeQuery = PredicateQuery(CompoundTerm("__nativeCode", emptyArray()))

class NativeCodeRule(name: String, arity: Int, definedAt: StackTraceElement, code: PrologBuiltinImplementation) : Rule(
    CompoundTerm(name, builtinArgumentVariables.sliceArray(0 until arity)),
    nativeCodeQuery
) {
    private val invocationStackFrame = definedAt
    private val stringRepresentation = """$head :- __nativeCode("${invocationStackFrame.fileName}:${invocationStackFrame.lineNumber}")"""

    private val builtinStackFrame = PrologStackTraceElement(
        head,
        invocationStackFrame.prologSourceInformation,
        "$name/$arity native implementation (${definedAt.fileName}:${definedAt.lineNumber})"
    )

    val callDirectly: PrologBuiltinImplementation = code

    override val fulfill: suspend LazySequenceBuilder<Unification>.(CompoundTerm, ProofSearchContext) -> Unit = { other, context ->
        if (head.arity == other.arity && head.name == other.name) {
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

fun nativeRule(name: String, arity: Int, code: PrologBuiltinImplementation): NativeCodeRule {
    val definedAt = getInvocationStackFrame()
    return NativeCodeRule(name, arity, definedAt, code)
}

fun nativeRule(name: String, arity: Int, definedAt: StackTraceElement, code: PrologBuiltinImplementation): NativeCodeRule {
    return NativeCodeRule(name, arity, definedAt, code)
}

fun nativeLibrary(name: String, initCode: NativeLibraryBuilder.() -> Any?): Library = NativeLibraryBuilder.build(name, initCode)

class NativeLibraryBuilder {
    private val clauses = mutableListOf<Clause>()
    private val dynamics = mutableSetOf<ClauseIndicator>()
    private val opRegistry = DefaultOperatorRegistry()

    fun add(clause: Clause) {
        clauses.add(clause)
    }

    fun defineOperator(def: OperatorDefinition) {
        opRegistry.defineOperator(def)
    }

    operator fun String.div(arity: Int) = ClauseIndicator.of(this, arity)

    private fun build(name: String): Library {
        return Library(
            name,
            clauses,
            dynamics,
            opRegistry
        )
    }

    companion object {
        internal fun build(name: String, initCode: NativeLibraryBuilder.() -> Any?): Library {
            val builder = NativeLibraryBuilder()
            builder.initCode()
            return builder.build(name)
        }
    }
}