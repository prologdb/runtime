package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import kotlin.math.pow

open class Integer(val value: Long) : Number {

    override fun plus(other: Number) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value + other.value)
            is Decimal -> Decimal(this.value.toDouble() + other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun minus(other: Number) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value - other.value)
            is Decimal -> Decimal(this.value.toDouble() - other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun times(other: Number) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value * other.value)
            is Decimal -> Decimal(this.value.toDouble() * other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun div(other: Number) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value / other.value)
            is Decimal -> Decimal(this.value.toDouble() / other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun rem(other: Number) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value % other.value)
            is Decimal -> Decimal(this.value.toDouble() % other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun toThe(other: Number): Number {
        return when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value.toDouble().pow(other.value.toDouble()).toLong())
            is Decimal -> Decimal(this.value.toDouble().pow(other.value))
            else -> throw PrologRuntimeException("Unsupported type of number")
        }
    }

    override fun unaryPlus(): Number = Integer.createUsingStringOptimizerCache(+this.value)

    override fun unaryMinus(): Number = Integer.createUsingStringOptimizerCache(-this.value)

    override fun compareTo(other: Number) =
        when(other) {
            is Integer -> this.value.compareTo(other.value)
            is Decimal -> this.value.compareTo(other.value)
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

    override fun toString() = value.toString()

    companion object {
        /* The integers 0 through 65536 are cached: these are used for
           strings and this avoids a lot of memory overhead. Also, this
           likely applies to other uses-cases where small numbers are used.
         */
        private val cache: MutableMap<Short, Integer> = mutableMapOf()

        fun createUsingStringOptimizerCache(value: Long): Integer {
            if (value > Short.MAX_VALUE || value < Short.MIN_VALUE) {
                return Integer(value) // constructor invocation
            } else {
                val valueAsShort = value.toShort()
                return cache[valueAsShort] ?: { val r = Integer(value); cache[valueAsShort] = r; r }()
            }
        }
    }
}