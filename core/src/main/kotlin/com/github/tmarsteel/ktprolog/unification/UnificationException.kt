package com.github.tmarsteel.ktprolog.unification

open class UnificationException(message: String, override val cause: Throwable? = null) : RuntimeException(message)