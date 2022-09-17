package com.github.prologdb.runtime.unification

class NameError(message: String, override val cause: Throwable? = null) : RuntimeException(message)