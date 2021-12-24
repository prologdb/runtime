package com.github.prologdb.runtime.term

@Retention(AnnotationRetention.RUNTIME)
@Target(AnnotationTarget.CLASS)
annotation class PrologTypeName(val value: String)
