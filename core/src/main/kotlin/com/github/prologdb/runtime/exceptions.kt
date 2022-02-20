@file:JvmName("PrologExceptionUtils")
package com.github.prologdb.runtime

import com.github.prologdb.runtime.exception.PrologStackTraceElement
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.prologTypeName

/**
 * An exception related to, but not limited to, parsing and interpreting prolog programs.
 */
abstract class PrologException(message: String, override val cause: Throwable? = null) : RuntimeException(message) {
    private val _prologStackTrace = mutableListOf<PrologStackTraceElement>()

    fun addPrologStackFrame(frameInfo: PrologStackTraceElement) {
        _prologStackTrace.add(frameInfo)
    }

    val prologStackTrace: List<PrologStackTraceElement> = _prologStackTrace
}

open class PrologInternalError(message: String, cause: Throwable? = null) : PrologException(message, cause)

open class PredicateNotDynamicException(val indicator: FullyQualifiedClauseIndicator, cause: Throwable? = null) : PrologException("Predicate $indicator is not dynamic", cause)

open class PrologPermissionError(message: String, cause: Throwable? = null) : PrologException(message, cause)

open class TermNotAssertableException(message: String) : PrologException(message)

open class PredicateNotDefinedException(
    val indicator: ClauseIndicator,
    val inContextOfModule: Module,
    message: String? = null
) : PrologException(
    message ?: "Predicate $indicator not defined in context of module ${inContextOfModule.name}"
)

open class PredicateNotExportedException(val fqi: FullyQualifiedClauseIndicator, inContextOfModule: Module) : PredicateNotDefinedException(
    fqi.indicator,
    inContextOfModule,
    "Predicate ${fqi.indicator} is not exported by module ${fqi.moduleName}"
)

open class ArgumentError(message: String, cause: Throwable? = null) : PrologException(message, cause)

class ArgumentTypeError(
    val predicate: ClauseIndicator?,
    val argumentIndex: Int,
    val actual: Term,
    vararg val expectedTypes: Class<out Term>
) : ArgumentError(StringBuilder().also { msg ->
    msg.append("Argument ")
    msg.append(argumentIndex + 1)

    if (predicate != null) {
        msg.append(" to ")
        msg.append(predicate.toString())
    }

    if (actual is Variable) {
        msg.append(" not sufficiently instantiated (expected ")
        msg.append(expectedTypes.expectedPhrase)
        msg.append(")")
    } else {
        msg.append(" must be ")
        msg.append(expectedTypes.expectedPhrase)
        msg.append(", got ")
        msg.append(actual.prologTypeName)
    }
}.toString()) {
    constructor(argumentIndex: Int, actual: Term, vararg expectedTypes: Class<out Term>) : this(
        null,
        argumentIndex,
        actual,
        *expectedTypes
    )

    private companion object {
        val Class<out Term>.expectedPhrase: String
            get() = if (Variable::class.java.isAssignableFrom(this)) "unbound" else "a $prologTypeName"
        val Array<out Class<out Term>>.expectedPhrase: String
            get() = when (this.size) {
                0, 1 -> single().expectedPhrase
                else -> {
                    val most = take(size - 1).joinToString(
                        transform = { it.expectedPhrase },
                        separator = ", "
                    )
                    "$most or ${last().expectedPhrase}"
                }
            }
    }
}
