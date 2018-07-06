package com.github.prologdb.runtime.term

interface PrologNumber : Term {
    operator fun plus(other: PrologNumber): PrologNumber
    operator fun minus(other: PrologNumber): PrologNumber
    operator fun times(other: PrologNumber): PrologNumber
    operator fun div(other: PrologNumber): PrologNumber
    operator fun rem(other: PrologNumber): PrologNumber
    infix fun toThe(other: PrologNumber): PrologNumber
    operator fun compareTo(other: PrologNumber) : Int

    operator fun unaryPlus(): PrologNumber
    operator fun unaryMinus(): PrologNumber

    val isInteger: Boolean

    override val prologTypeName
        get() = "number"

    /**
     * If this is an integer, returns its value. Otherwise rounds
     * to the nearest integer and returns that
     */
    fun toInteger(): Long

    /**
     * Converts this number to a [Double] and returns it.
     */
    fun toDecimal(): Double
}