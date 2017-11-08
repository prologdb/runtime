package com.github.tmarsteel.ktprolog.term

interface Number : Term {
    operator fun plus(other: Number): Number
    operator fun minus(other: Number): Number
    operator fun times(other: Number): Number
    operator fun div(other: Number): Number
    operator fun rem(other: Number): Number
    operator fun compareTo(other: Number) : Int
}