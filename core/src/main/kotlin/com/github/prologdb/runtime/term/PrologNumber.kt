package com.github.prologdb.runtime.term

/**
 * Numbers in prolog.
 *
 * On implementing HasPrologSource: extending the number term types does not
 * make sense in any way and the calculations can also benefit from not going
 * through dynamic-dispatch getters to obtain the native prolog nubmers to actually
 * do the calculation. This is a problem for the parser, however: it cannot create
 * a subtype of number to attach the source information to. So this class offers
 * a way to store the source information.
 */
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