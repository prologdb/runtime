package com.github.prologdb.runtime

interface PrologSourceInformation {
    /** The source file name, null if unknown */
    val sourceFileName: String?

    /** The line number within [sourceFileName] at which the error occured, null if unknown */
    val sourceFileLine: Int?

    /** Of both this and the given object returns that with more information. */
    fun orElse(other: () -> PrologSourceInformation): PrologSourceInformation = this

    /** Of both this and the given object returns that with more information. */
    fun orElse(other: PrologSourceInformation): PrologSourceInformation = this
}

object NullSourceInformation : PrologSourceInformation {
    override val sourceFileLine: Int? = null
    override val sourceFileName: String? = null
    override fun orElse(other: () -> PrologSourceInformation) = other()
    override fun orElse(other: PrologSourceInformation) = other
}