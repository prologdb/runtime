package com.github.prologdb.runtime.stdlib

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.PrologInternalError
import com.github.prologdb.runtime.PrologInvocationContractViolationException
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.builtin.prologSourceInformation
import com.github.prologdb.runtime.exception.PrologStackTraceElement
import com.github.prologdb.runtime.proofsearch.PrologCallableFulfill
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

internal val A = Variable("A")
internal val B = Variable("B")
internal val C = Variable("C")
internal val X = Variable("X")

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
    Variable("_Arg7"),
    Variable("_Arg8"),
    Variable("_Arg9")
)

/**
 * This query is used as a placeholder for builtins where an instance of [Query] is required
 * but the actual query never gets invoked because kotlin code implements the builtin.
 */
private val nativeCodeQuery = PredicateInvocationQuery(CompoundTerm("__nativeCode", emptyArray()))

typealias NativePredicateFulfill = suspend LazySequenceBuilder<Unification>.(TypedPredicateArguments, ProofSearchContext) -> Unification?

class NativeCodeRule(name: String, arity: Int, definedAt: StackTraceElement, code: NativePredicateFulfill) : Rule(
    CompoundTerm(name, builtinArgumentVariables.sliceArray(0 until arity)),
    nativeCodeQuery
) {
    private val invocationStackFrame = definedAt
    private val stringRepresentation = """$head :- __nativeCode("${definedAt.fileName}:${definedAt.lineNumber}")"""
    private val indicator = ClauseIndicator.of(head)

    private val builtinStackFrame = PrologStackTraceElement(
        head,
        invocationStackFrame.prologSourceInformation,
        null,
        "$indicator native implementation (${definedAt.fileName}:${definedAt.lineNumber})"
    )

    override val fulfill: PrologCallableFulfill = { arguments, context ->
        if (head.arity == arguments.size) {
            val typeSafeArguments = TypedPredicateArguments(indicator, arguments)
            try {
                code(this, typeSafeArguments, context)
            } catch (ex: PrologException) {
                if (ex is PrologInvocationContractViolationException && ex.indicator == null) {
                    ex.fillIndicator(indicator)
                }
                ex.addPrologStackFrame(builtinStackFrame)
                throw ex
            } catch (ex: Throwable) {
                val newEx = PrologInternalError(ex.message ?: "", ex)
                newEx.addPrologStackFrame(builtinStackFrame)

                throw newEx
            }
        } else null
    }

    override fun toString() = stringRepresentation
}

fun nativeRule(name: String, arity: Int, code: NativePredicateFulfill): NativeCodeRule {
    val definedAt = getInvocationStackFrame()
    return NativeCodeRule(name, arity, definedAt, code)
}

fun nativeRule(name: String, arity: Int, definedAt: StackTraceElement, code: NativePredicateFulfill): NativeCodeRule {
    return NativeCodeRule(name, arity, definedAt, code)
}

inline fun <reified From : Term, reified To : Term> nativeConversionRule(name: String, crossinline aToB: (From) -> To, crossinline bToA: (To) -> From) : NativeCodeRule = nativeRule(name, 2, getInvocationStackFrame()) { args, ctxt ->
    val inputForA = args[0]
    val inputForB = args[1]

    if (inputForA !is Variable) {
        if (inputForA !is From) {
            throw ArgumentTypeError(0, inputForA, From::class.java)
        }

        val convertedB = aToB(inputForA)
        return@nativeRule convertedB.unify(inputForB, ctxt.randomVariableScope)
    }

    if (inputForB !is Variable) {
        if (inputForB !is To) {
            throw ArgumentTypeError(1, inputForB, To::class.java)
        }

        val convertedA = bToA(inputForB)
        return@nativeRule  inputForA.unify(convertedA, ctxt.randomVariableScope)
    }

    throw PrologInvocationContractViolationException("At least one argument to ${args.indicator} must be instantiated")
}