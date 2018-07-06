package com.github.prologdb.runtime.term

interface Number : Term {
    operator fun plus(other: Number): Number
    operator fun minus(other: Number): Number
    operator fun times(other: Number): Number
    operator fun div(other: Number): Number
    operator fun rem(other: Number): Number
    infix fun toThe(other: Number): Number
    operator fun compareTo(other: Number) : Int

    operator fun unaryPlus(): Number
    operator fun unaryMinus(): Number

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