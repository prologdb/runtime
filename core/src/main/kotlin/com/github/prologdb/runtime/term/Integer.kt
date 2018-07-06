package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import kotlin.math.pow

open class Integer(val value: Long) : PrologNumber {

    override val isInteger = true

    override fun plus(other: PrologNumber) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value + other.value)
            is Decimal -> Decimal(this.value.toDouble() + other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun minus(other: PrologNumber) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value - other.value)
            is Decimal -> Decimal(this.value.toDouble() - other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun times(other: PrologNumber) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value * other.value)
            is Decimal -> Decimal(this.value.toDouble() * other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun div(other: PrologNumber) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value / other.value)
            is Decimal -> Decimal(this.value.toDouble() / other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun rem(other: PrologNumber) =
        when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value % other.value)
            is Decimal -> Decimal(this.value.toDouble() % other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun toThe(other: PrologNumber): PrologNumber {
        return when(other) {
            is Integer -> Integer.createUsingStringOptimizerCache(this.value.toDouble().pow(other.value.toDouble()).toLong())
            is Decimal -> Decimal(this.value.toDouble().pow(other.value))
            else -> throw PrologRuntimeException("Unsupported type of number")
        }
    }

    override fun unaryPlus(): PrologNumber = Integer.createUsingStringOptimizerCache(+this.value)

    override fun unaryMinus(): PrologNumber = Integer.createUsingStringOptimizerCache(-this.value)

    override fun toInteger(): Long = value

    override fun toDecimal(): Double = value.toDouble()

    override fun compareTo(other: PrologNumber) =
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

    override fun compareTo(other: Term): Int {
        when(other) {
            // Variables are, by category, lesser than numbers
            is Variable -> return 1

            // ISO prolog categorically sorts decimals before integers
            // as the author of this runtime, i deem this suboptimal.
            // this behaves identical to SWI prolog

            is Decimal -> {
                // compare mixed as floating point
                val thisAsDouble = toDecimal()
                if (thisAsDouble == other.value) return 1 // if equal, the floating point is lesser
                if (thisAsDouble > other.value) return 1
                return -1
            }

            is Integer -> {
                // this.value - other.value does NOT work here: it has to be converted to an integer
                // before return. That might change the result incorrectly.
                return this.value.compareTo(other.value)
            }

            // everything else is, by category, greater than numbers
            else -> return -1
        }
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