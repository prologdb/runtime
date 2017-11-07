package com.github.tmarsteel.ktprolog.term

import com.github.tmarsteel.ktprolog.PrologRuntimeException
import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.unification.Unification

open class Integer(val value: Long) : Number {

    override fun plus(other: Number) =
        when(other) {
            is Integer -> Integer(this.value + other.value)
            is Decimal -> Decimal(this.value.toDouble() + other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun minus(other: Number) =
        when(other) {
            is Integer -> Integer(this.value - other.value)
            is Decimal -> Decimal(this.value.toDouble() - other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun times(other: Number) =
        when(other) {
            is Integer -> Integer(this.value * other.value)
            is Decimal -> Decimal(this.value.toDouble() * other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun div(other: Number) =
        when(other) {
            is Integer -> Integer(this.value / other.value)
            is Decimal -> Decimal(this.value.toDouble() / other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun rem(other: Number) =
        when(other) {
            is Integer -> Integer(this.value % other.value)
            is Decimal -> Decimal(this.value.toDouble() % other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun toThePowerOf(other: Number) =
        when(other) {
            is Integer -> Decimal(Math.pow(this.value.toDouble(), other.value.toDouble()))
            is Decimal -> Decimal(Math.pow(this.value.toDouble(), other.value))
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is Integer) {
            if (rhs.value == value) {
                return Unification.TRUE
            } else {
                return Unification.FALSE
            }
        } else {
            return rhs.unify(this, randomVarsScope)
        }
    }

    override val variables = emptySet<Variable>()

    override fun substituteVariables(mapper: (Variable) -> Term) = this

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Integer) return false

        if (value != other.value) return false

        return true
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }
}