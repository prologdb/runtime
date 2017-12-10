package com.github.prologdb.runtime.unification

open class UnificationException(message: String, override val cause: Throwable? = null) : RuntimeException(message)