package com.github.prologdb.runtime.builtin

import com.github.prologdb.runtime.PrologSourceInformation

/**
 * @return the stackframe that resembles the invocation to the caller of this function.
 */
fun getInvocationStackFrame(): StackTraceElement = Thread.currentThread().stackTrace[3]

val StackTraceElement.prologSourceInformation: PrologSourceInformation
    get() = object : PrologSourceInformation {
        override val sourceFileName = fileName
        override val sourceFileLine = lineNumber
    }