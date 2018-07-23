package com.github.prologdb.runtime

import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.lazysequence.transformExceptionsOnRemaining
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

data class PrologStackTraceElement(
    val goalPredicate: Predicate,
    val sourceInformation: PrologSourceInformation
){
    override fun toString() = "\tat $goalPredicate (${sourceInformation.sourceFileName}:${sourceInformation.sourceFileLine})"
}

inline fun <T> prologTry(onErrorStackTraceElement: PrologStackTraceElement, code: () -> T): T {
    try {
        return code()
    }
    catch (ex: PrologException) {
        ex.addPrologStackFrame(onErrorStackTraceElement)
        throw ex
    }
}

fun <T> LazySequence<T>.prologTryOnRemaining(onErrorStackTraceElement: PrologStackTraceElement): LazySequence<T> {
    return this.transformExceptionsOnRemaining { e: PrologException ->
        e.addPrologStackFrame(onErrorStackTraceElement)
        e
    }
}