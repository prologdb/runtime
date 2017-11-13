package com.github.tmarsteel.ktprolog

/**
 * Models a runtime error in a prolog program.
 */
open class PrologRuntimeException(message: String, override val cause: Throwable? = null) : RuntimeException(message)