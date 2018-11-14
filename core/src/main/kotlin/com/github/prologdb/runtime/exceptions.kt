package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.transformExceptionsOnRemaining
import com.github.prologdb.runtime.knowledge.library.ClauseIndicator
import com.github.prologdb.runtime.term.Predicate

/**
 * An exception related to, but not limited to, parsing and interpreting prolog programs.
 */
open class PrologException(message: String, override val cause: Throwable? = null) : RuntimeException(message) {
    private val _prologStackTrace = mutableListOf<PrologStackTraceElement>()

    fun addPrologStackFrame(frameInfo: PrologStackTraceElement) {
        _prologStackTrace.add(frameInfo)
    }

    val prologStackTrace: List<PrologStackTraceElement> = _prologStackTrace
}

/**
 * Thrown when errors or warnings occur during the interpretation of a prolog program.
 */
open class PrologRuntimeException(message: String, cause: Throwable? = null) : PrologException(message, cause)

open class PredicateNotDynamicException(private val indicator: ClauseIndicator, message: String, cause: Throwable? = null) : PrologRuntimeException(message, cause) {
    constructor(indicator: ClauseIndicator) : this(
        indicator,
        "Predicate $indicator is not dynamic"
    )
}

/**
 * Thrown when a prolog system is asked to handle a directive that is not supported. Directives are instances of
 * `:-/1`; Usually, attempting to handle other predicates as directives leads to this error.
 */
class IllegalDirectiveException(message: String, cause: Throwable? = null) : PrologException(message, cause) {
    constructor(rejectedDirective: Predicate) : this(
            if (rejectedDirective.name != ":-" || rejectedDirective.arity != 1) {
                "Directives must be instances of :-/1"
            } else {
                "Unsupported or illegal directive: $rejectedDirective"
            }
    )
}

class PrologPermissionError(message: String, cause: Throwable? = null) : PrologRuntimeException(message, cause)

data class PrologStackTraceElement(
    val goalPredicate: Predicate,
    val sourceInformation: PrologSourceInformation
){
    override fun toString() = "$goalPredicate   ${sourceInformation.sourceFileName}:${sourceInformation.sourceFileLine}"
}

/**
 * Runs the code; if it throws a [PrologException], amends the [PrologException.stackTrace] with
 * the given [PrologStackTraceElement].
 */
inline fun <T> prologTry(onErrorStackTraceElement: PrologStackTraceElement, code: () -> T): T {
    try {
        return code()
    }
    catch (ex: PrologException) {
        ex.addPrologStackFrame(onErrorStackTraceElement)
        throw ex
    }
}

/**
 * Maps the sequence; for every [PrologException] thrown from the original sequence as a result
 * of invocations to [LazySequence.tryAdvance] the [PrologException.prologStackTrace] is amended
 * with the given stack trace element (as if each call to `tryAdvance` were wrapped in [prologTry]).
 *
 * This has to be included in all calls made by the interpreter that should show up in the prolog
 * stacktraces; this is definitely for all calls the interpreter starts on behalf of user code.
 * Calls the interpreter does on its own behalf **may** use this.
 */
fun <T> LazySequence<T>.amendExceptionsWithStackTraceOnRemaining(onErrorStackTraceElement: PrologStackTraceElement): LazySequence<T> {
    return this.transformExceptionsOnRemaining { e: PrologException ->
        e.addPrologStackFrame(onErrorStackTraceElement)
        e
    }
}