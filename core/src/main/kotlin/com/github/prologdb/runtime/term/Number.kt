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
}