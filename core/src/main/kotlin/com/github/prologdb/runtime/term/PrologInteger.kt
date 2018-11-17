package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import kotlin.math.pow

open class PrologInteger(
    val value: Long,
    override val sourceInformation: PrologSourceInformation = NullSourceInformation
) : PrologNumber {

    override val isInteger = true

    override fun plus(other: PrologNumber) =
        when(other) {
            is PrologInteger -> PrologInteger.createUsingStringOptimizerCache(this.value + other.value)
            is PrologDecimal -> PrologDecimal(this.value.toDouble() + other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun minus(other: PrologNumber) =
        when(other) {
            is PrologInteger -> PrologInteger.createUsingStringOptimizerCache(this.value - other.value)
            is PrologDecimal -> PrologDecimal(this.value.toDouble() - other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun times(other: PrologNumber) =
        when(other) {
            is PrologInteger -> PrologInteger.createUsingStringOptimizerCache(this.value * other.value)
            is PrologDecimal -> PrologDecimal(this.value.toDouble() * other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun div(other: PrologNumber) =
        when(other) {
            is PrologInteger -> PrologInteger.createUsingStringOptimizerCache(this.value / other.value)
            is PrologDecimal -> PrologDecimal(this.value.toDouble() / other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun rem(other: PrologNumber) =
        when(other) {
            is PrologInteger -> PrologInteger.createUsingStringOptimizerCache(this.value % other.value)
            is PrologDecimal -> PrologDecimal(this.value.toDouble() % other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun toThe(other: PrologNumber): PrologNumber {
        return when(other) {
            is PrologInteger -> PrologInteger.createUsingStringOptimizerCache(this.value.toDouble().pow(other.value.toDouble()).toLong())
            is PrologDecimal -> PrologDecimal(this.value.toDouble().pow(other.value))
            else -> throw PrologRuntimeException("Unsupported type of number")
        }
    }

    override fun unaryPlus(): PrologNumber = PrologInteger.createUsingStringOptimizerCache(+this.value)

    override fun unaryMinus(): PrologNumber = PrologInteger.createUsingStringOptimizerCache(-this.value)

    override fun toInteger(): Long = value

    override fun toDecimal(): Double = value.toDouble()

    override fun compareTo(other: PrologNumber) =
        when(other) {
            is PrologInteger -> this.value.compareTo(other.value)
            is PrologDecimal -> this.value.compareTo(other.value)
            else -> throw PrologRuntimeException("Unsupported type of number")
        }

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is PrologInteger) {
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
        if (other !is PrologInteger) return false

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

            is PrologDecimal -> {
                // compare mixed as floating point
                val thisAsDouble = toDecimal()
                if (thisAsDouble == other.value) return 1 // if equal, the floating point is lesser
                if (thisAsDouble > other.value) return 1
                return -1
            }

            is PrologInteger -> {
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
        private val CACHE: MutableMap<Short, PrologInteger> = mutableMapOf()

        fun createUsingStringOptimizerCache(value: Long): PrologInteger {
            if (value > Short.MAX_VALUE || value < Short.MIN_VALUE) {
                return PrologInteger(value) // constructor invocation
            } else {
                val valueAsShort = value.toShort()
                return CACHE[valueAsShort] ?: { val r = PrologInteger(value); CACHE[valueAsShort] = r; r }()
            }
        }
    }
}