package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.PrologRuntimeException
import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.unification.Unification

open class Decimal(val value: Double) : Number {
    override fun plus(other: Number) =
        when(other) {
            is Decimal -> Decimal(value + other.value)
            is Integer -> Decimal(value + other.value.toDouble())
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun minus(other: Number) =
        when(other) {
            is Decimal -> Decimal(value - other.value)
            is Integer -> Decimal(value - other.value.toDouble())
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun times(other: Number) =
        when(other) {
            is Decimal -> Decimal(value * other.value)
            is Integer -> Decimal(value * other.value.toDouble())
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun div(other: Number) =
        when(other) {
            is Decimal -> Decimal(value / other.value)
            is Integer -> Decimal(value / other.value.toDouble())
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun rem(other: Number) =
        when(other) {
            is Decimal -> Decimal(value % other.value)
            is Integer -> Decimal(value % other.value.toDouble())
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun toThePowerOf(other: Number) =
        when(other) {
            is Decimal -> Decimal(Math.pow(value, other.value))
            is Integer -> Decimal(Math.pow(value, other.value.toDouble()))
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Decimal) {
            if (rhs.value == value) {
                return Unification.TRUE
            } else {
                return Unification.FALSE
            }
        } else if (rhs is Integer) {
            if (rhs.value.toDouble() == value) {
                return Unification.TRUE
            } else {
                return Unification.FALSE
            }
        } else {
            return rhs.unify(this, randomVarsScope)
        }
    }

    override val variables: Set<Variable> = emptySet()

    override fun substituteVariables(mapper: (Variable) -> Term) = this

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Decimal) return false

        if (value != other.value) return false

        return true
    }

    override fun hashCode() = value.hashCode()

    override fun toString() = value.toString()
}