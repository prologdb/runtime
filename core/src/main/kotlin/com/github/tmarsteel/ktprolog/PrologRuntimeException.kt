package com.github.tmarsteel.ktprolog

/**
 * Models a runtime error in a prolog program.
 */
class PrologRuntimeException(message: String, override val cause: Throwable? = null) : Throwable(message)