@file:JvmName("PrologExceptionUtils")
package com.github.prologdb.runtime

import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.term.prologTypeName

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

open class PredicateNotDynamicException(val indicator: FullyQualifiedClauseIndicator, cause: Throwable? = null) : PrologRuntimeException("Predicate $indicator is not dynamic", cause)

open class PrologPermissionError(message: String, cause: Throwable? = null) : PrologRuntimeException(message, cause)

class ArgumentTypeError(
    val predicate: ClauseIndicator?,
    val argumentIndex: Int,
    val actual: Term,
    vararg val expectedTypes: Class<out Term>
) : PrologRuntimeException(StringBuilder().also { msg ->
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

data class PrologStackTraceElement @JvmOverloads constructor(
    val goal: CompoundTerm,
    val sourceInformation: PrologSourceInformation,
    val module: Module? = null,
    val toStringOverride: String? = null
) {
    override fun toString() = toStringOverride ?: run {
        val modulePrefix = if (module == null) "" else "module ${module.name}, "
        "$goal   $modulePrefix${sourceInformation.sourceFileName}:${sourceInformation.sourceFileLine}"
    }
}

/**
 * Runs the code; if it throws a [PrologException], amends the [PrologException.stackTrace] with
 * the given [PrologStackTraceElement].
 */
inline fun <T> prologTry(crossinline onErrorStackTraceElement: () -> PrologStackTraceElement, code: () -> T): T {
    try {
        return code()
    } catch (ex: PrologException) {
        ex.addPrologStackFrame(onErrorStackTraceElement())
        throw ex
    }
}
