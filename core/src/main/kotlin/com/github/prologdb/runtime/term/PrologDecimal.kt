package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import kotlin.math.pow
import kotlin.math.roundToLong

@PrologTypeName("decimal")
class PrologDecimal(
    val value: Double
) : PrologNumber {
    override val isInteger = false

    override fun plus(other: PrologNumber) =
        when(other) {
            is PrologDecimal -> PrologDecimal(value + other.value)
            is PrologInteger -> PrologDecimal(value + other.value.toDouble())
        }

    override fun minus(other: PrologNumber) =
        when(other) {
            is PrologDecimal -> PrologDecimal(value - other.value)
            is PrologInteger -> PrologDecimal(value - other.value.toDouble())
        }

    override fun times(other: PrologNumber) =
        when(other) {
            is PrologDecimal -> PrologDecimal(value * other.value)
            is PrologInteger -> PrologDecimal(value * other.value.toDouble())
        }

    override fun div(other: PrologNumber) =
        when(other) {
            is PrologDecimal -> PrologDecimal(value / other.value)
            is PrologInteger -> PrologDecimal(value / other.value.toDouble())
        }

    override fun rem(other: PrologNumber) =
        when(other) {
            is PrologDecimal -> PrologDecimal(value % other.value)
            is PrologInteger -> PrologDecimal(value % other.value.toDouble())
        }

    override fun toThe(other: PrologNumber): PrologNumber {
        return when(other) {
            is PrologDecimal -> PrologDecimal(this.value.pow(other.value))
            is PrologInteger -> PrologDecimal(this.value.pow(other.value.toDouble()))
        }
    }

    override fun unaryPlus(): PrologNumber = PrologDecimal(+this.value)

    override fun unaryMinus(): PrologNumber = PrologDecimal(-this.value)

    override fun asPrologDecimal(): PrologDecimal = this

    override fun toInteger(): Long = value.roundToLong()

    override fun toDecimal(): Double = value

    override fun ceil(): PrologInteger {
        return PrologInteger(kotlin.math.ceil(this.value).toLong())
    }

    override fun floor(): PrologInteger {
        return PrologInteger(kotlin.math.floor(this.value).toLong())
    }

    override fun compareTo(other: PrologNumber) =
        when(other) {
            is PrologInteger -> this.value.compareTo(other.value)
            is PrologDecimal -> this.value.compareTo(other.value)
        }

    override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        if (rhs is PrologDecimal) {
            if (rhs.value == value) {
                return Unification.TRUE
            } else {
                return Unification.FALSE
            }
        } else if (rhs is PrologInteger) {
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
    override val isGround = true

    override fun substituteVariables(mapper: (Variable) -> Term) = this

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is PrologDecimal) return false

        if (value != other.value) return false

        return true
    }

    override fun hashCode() = value.hashCode()

    override fun compareTo(other: Term): Int {
        when(other) {
            // Variables are, by category, lesser than numbers
            is Variable -> return 1

            // ISO prolog categorically sorts decimals before integers
            // as the author of this runtime, i deem this suboptimal.
            // this behaves identical to SWI prolog

            is PrologDecimal -> {
                return this.value.compareTo(other.value)
            }

            is PrologInteger -> {
                // compare mixed as floating point
                val integerAsDouble = other.toDecimal()
                if (this.value == integerAsDouble) return -1 // if equal, the floating point is lesser
                if (this.value >  integerAsDouble) return 1
                return -1
            }

            // everything else is, by category, greater than numbers
            else -> return -1
        }
    }

    override fun toString() = value.toString()

    override var sourceInformation: PrologSourceInformation = NullSourceInformation
}
