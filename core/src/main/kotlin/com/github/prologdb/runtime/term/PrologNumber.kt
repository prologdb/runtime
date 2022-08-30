package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import org.apfloat.Apfloat
import org.apfloat.ApfloatMath
import org.apfloat.InfiniteExpansionException
import java.math.BigInteger
import java.math.RoundingMode

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
@PrologTypeName("number")
sealed class PrologNumber : Term {
    final override val variables = emptySet<Variable>()
    final override val isGround = true
    final override var sourceInformation: PrologSourceInformation = NullSourceInformation

    abstract operator fun plus(other: PrologNumber): PrologNumber
    abstract operator fun minus(other: PrologNumber): PrologNumber
    abstract operator fun times(other: PrologNumber): PrologNumber
    abstract operator fun div(other: PrologNumber): PrologNumber
    abstract operator fun rem(other: PrologNumber): PrologNumber
    abstract infix fun toThe(other: PrologNumber): PrologNumber
    abstract operator fun compareTo(other: PrologNumber) : Int

    abstract operator fun unaryPlus(): PrologNumber
    abstract operator fun unaryMinus(): PrologNumber

    abstract fun ceil(): PrologNumber
    abstract fun floor(): PrologNumber

    abstract fun sqrt(): PrologNumber

    abstract val isInteger: Boolean

    /**
     * If this is an integer, returns its value. Otherwise rounds
     * to the nearest integer and returns that.
     * @throws ArithmeticException if the value of this number overflows the range of [Long]
     */
    abstract fun toInteger(): Long

    /**
     * Converts this number to a [Double] and returns it.
     * @throws ArithmeticException if the value of this number overflows the range of [Double]
     */
    abstract fun toDecimal(): Double

    final override fun unify(rhs: Term, randomVarsScope: RandomVariableScope): Unification? {
        return when (rhs) {
            is PrologNumber -> Unification.whether(this.compareTo(rhs as PrologNumber) == 0)
            else -> rhs.unify(this, randomVarsScope)
        }
    }

    final override fun compareTo(other: Term): Int = when(other) {
        // Variables are, by category, lesser than numbers
        is Variable -> 1
        is PrologNumber -> compareTo(other as PrologNumber)
        // everything else is, by category, greater than numbers
        else -> -1
    }

    final override fun equals(other: Any?): Boolean {
        if (this === other) {
            return true
        }

        if (other is PrologNumber) {
            return this.compareTo(other as PrologNumber) == 0
        }

        return false
    }

    final override fun substituteVariables(mapper: (Variable) -> Term): Term = this

    companion object {
        // TODO: string optimizer cache
        private val LONG_MAX_APFLOAT = Apfloat(Long.MAX_VALUE)
        operator fun invoke(value: Byte): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: UByte): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: Short): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: UShort): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: Int): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: UInt): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: Long): PrologNumber = PrologLongInteger(value)
        operator fun invoke(value: ULong): PrologNumber {
            val raw = value.toLong()
            if (raw >= 0) {
                return PrologLongInteger(raw)
            }

            var asApfloat = LONG_MAX_APFLOAT
            var remainder = value - Long.MAX_VALUE.toULong()
            if (remainder > Long.MAX_VALUE.toULong()) {
                asApfloat = asApfloat.add(LONG_MAX_APFLOAT)
                remainder -= Long.MAX_VALUE.toULong()
            }

            asApfloat = asApfloat.add(Apfloat(remainder.toLong()))
            return PrologBigNumber(asApfloat)
        }
        operator fun invoke(value: Float): PrologNumber = PrologBigNumber(Apfloat(value))
        operator fun invoke(value: Double): PrologNumber = PrologBigNumber(Apfloat(value))

        /**
         * @throws NumberFormatException
         */
        operator fun invoke(value: String): PrologNumber {
            return try {
                PrologLongInteger(value.toLong())
            } catch (longEx: NumberFormatException) {
                try {
                    PrologBigNumber(value)
                }
                catch (bigEx: NumberFormatException) {
                    bigEx.addSuppressed(longEx)
                    throw bigEx
                }
            }
        }
    }
}

class PrologInteger {
    companion object {
        @Deprecated("use PrologNumber.Companion.invoke instead", replaceWith = ReplaceWith("PrologNumber(value)", imports = ["com.github.prologdb.runtime.term.PrologNumber"]))
        fun createUsingStringOptimizerCache(value: Long): PrologNumber = error("not implemented")

        @Deprecated("use PrologNumber.Companion.invoke instead", replaceWith = ReplaceWith("PrologNumber(value)", imports = ["com.github.prologdb.runtime.term.PrologNumber"]))
        operator fun invoke(value: Number): PrologNumber = error("not implemented")
    }

}

@PrologTypeName("number")
class PrologLongInteger(
    val value: Long
) : PrologNumber() {
    override val isInteger = true
    override fun plus(other: PrologNumber) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, Math::addExact, Apfloat::add)
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).add(other.value))
        }

    override fun minus(other: PrologNumber) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, Math::subtractExact, Apfloat::subtract)
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).subtract(other.value))
        }

    override fun times(other: PrologNumber) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, Math::multiplyExact, Apfloat::multiply)
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).multiply(other.value))
        }

    override fun div(other: PrologNumber) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, ::divideExact, Apfloat::divideWithFinitePrecision)
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).divideWithFinitePrecision(other.value))
        }

    override fun rem(other: PrologNumber) =
        when(other) {
            is PrologLongInteger -> PrologLongInteger(this.value % other.value)
            is PrologBigNumber -> {
                val result = Apfloat(this.value).mod(other.value)
                try {
                    PrologLongInteger(result.longValueExact())
                }
                catch (_: ArithmeticException) {
                    PrologBigNumber(result)
                }
            }
        }

    override fun toThe(other: PrologNumber): PrologNumber {
        return when(other) {
            is PrologLongInteger -> PrologBigNumber(Apfloat(this.value).powSupportingNegativeBase(Apfloat(other.value)))
            is PrologBigNumber -> PrologBigNumber(ApfloatMath.round(ApfloatMath.pow(Apfloat(this.value, PrologBigNumber.PRECISION + 1), other.value), PrologBigNumber.PRECISION, RoundingMode.HALF_UP))
        }
    }

    override fun ceil() = this
    override fun floor() = this
    override fun unaryPlus() = if (this.value >= 0) this else PrologLongInteger(+this.value)
    override fun unaryMinus() = PrologLongInteger(-this.value)
    override fun toInteger(): Long = value
    override fun toDecimal(): Double = value.toDouble()
    override fun sqrt() = PrologBigNumber(ApfloatMath.round(ApfloatMath.sqrt(Apfloat(this.value, PrologBigNumber.PRECISION + 1)), PrologBigNumber.PRECISION, RoundingMode.HALF_UP))

    override fun compareTo(other: PrologNumber) = when(other) {
        is PrologLongInteger -> this.value.compareTo(other.value)
        is PrologBigNumber -> Apfloat(this.value).compareTo(other.value)
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }

    override fun toString() = value.toString()
}

@PrologTypeName("number")
class PrologBigNumber internal constructor(internal val value: Apfloat) : PrologNumber() {
    /**
     * @param value the decimal point is marked with `'.'`. For `[radix] <= 14`, `'e'`
     *              and `'E'` can be used to denote scale in scientific notation (e.g. 1.24e-16);
     *              from radix `14` and above, `e` and `E` are interpreted as part of the number.
     * @throws NumberFormatException
     */
    @JvmOverloads
    constructor(value: String, radix: Int = 10) : this(Apfloat(value, Apfloat.INFINITE, radix).toRadix(10))

    /**
     * Interprets the [len] bytes in [data] (starting at index [offset]) as a twos-complement representation
     * of a positive integer.
     * @param data twos-complement representation of the value with big endianess
     * @param signum Signum of the value (-1, 0 or 1 for negative, zero or positive numbers respectively)
     * @param scale Scale that will convert the integer into a decimal value; the actual value of
     *              the resulting [PrologBigNumber] will be `intValue * pow(10, scale)`
     */
    constructor(data: ByteArray, off: Int, len: Int, signum: Int, scale: Long = 0) : this({
        val bigInt = BigInteger(signum, data, off, len)
        val unscaled = Apfloat(bigInt, Apfloat.INFINITE, 10)
        ApfloatMath.scale(unscaled, scale)
    }())

    override fun plus(other: PrologNumber) = combineSimple(other, Apfloat::add)
    override fun minus(other: PrologNumber) = combineSimple(other, Apfloat::subtract)
    override fun times(other: PrologNumber) = combineSimple(other, Apfloat::multiply)
    override fun div(other: PrologNumber) = combineSimple(other, Apfloat::divideWithFinitePrecision)
    override fun rem(other: PrologNumber) = combineSimple(other, Apfloat::mod)
    override fun toThe(other: PrologNumber) = combineSimple(other, Apfloat::powSupportingNegativeBase)
    override fun compareTo(other: PrologNumber) = when (other) {
        is PrologLongInteger -> this.value.compareTo(Apfloat(other.value))
        is PrologBigNumber -> this.value.compareTo(other.value)
    }
    override fun unaryPlus() = if (this.value.signum() >= 0) this else PrologBigNumber(this.value.negate())
    override fun unaryMinus() = PrologBigNumber(this.value.negate())
    override fun ceil() = PrologBigNumber(this.value.ceil())
    override fun floor() = PrologBigNumber(this.value.floor())
    override fun sqrt(): PrologNumber {
        return try {
            PrologBigNumber(ApfloatMath.sqrt(this.value))
        } catch (_: InfiniteExpansionException) {
            PrologBigNumber(ApfloatMath.round(ApfloatMath.sqrt(this.value.precision(PRECISION + 1)), PRECISION, RoundingMode.HALF_UP))
        }
    }
    override val isInteger = this.value.isInteger
    override fun toInteger() = this.value.longValueExact()
    override fun toDecimal() = this.value.toDouble().takeUnless { it.isInfinite() || it.isNaN() } ?: throw ArithmeticException("overflow")
    override fun hashCode() = this.value.hashCode()
    override fun toString(): String = this.value.toString(true)

    /**
     * Creates an easily serializable form of this number. Use together with the `(ByteArray, Int, Int, Int, Long)`
     * constructor.
     * @return [Triple.first]: twos-complement representation of the number, [Triple.second]: the signum, [Triple.third]: the scale of the number
     */
    fun serialize(): Triple<ByteArray, Int, Long> {
        val withoutScale = ApfloatMath.scale(value, -value.scale())
        val asString = withoutScale.toString(true)
        val asByteArray = BigInteger(asString).toByteArray()
        return Triple(asByteArray, value.signum(), value.scale())
    }

    private inline fun combineSimple(other: PrologNumber, crossinline operation: (Apfloat, Apfloat) -> (Apfloat)): PrologNumber {
        return when (other) {
            is PrologLongInteger -> PrologBigNumber(operation(this.value, Apfloat(other.value)))
            is PrologBigNumber -> PrologBigNumber(operation(this.value, other.value))
        }
    }

    companion object {
        // TODO: put into ProofSearchContext
        const val PRECISION = 50L
    }
}

private fun divideExact(divident: Long, divisor: Long): Long {
    if (divident % divisor != 0L) {
        throw ArithmeticException("Integer division is not accurate")
    }

    return divident / divisor
}

/**
 * @param simpleCombinator must throw [ArithmeticException] if the opration is not exact. Use method references from [Math]
 */
private inline fun Long.combineExact(
    other: Long,
    crossinline simpleCombinator: (Long, Long) -> Long,
    crossinline apfloatCombinator: (Apfloat, Apfloat) -> Apfloat,
): PrologNumber {
    return try {
        PrologLongInteger(simpleCombinator(this, other))
    }
    catch (_: ArithmeticException) {
        PrologBigNumber(
            ApfloatMath.round(
                apfloatCombinator(Apfloat(this, PrologBigNumber.PRECISION + 1), Apfloat(other, PrologBigNumber.PRECISION + 1)),
                PrologBigNumber.PRECISION,
                RoundingMode.HALF_UP,
            )
        )
    }
}

private fun Apfloat.powSupportingNegativeBase(exponent: Apfloat): Apfloat {
    // ApfloatMath rejects the operation if the base is negative as the result would be a complex number
    // however, all prolog systems agree that `0.25 is (-0.5)^2`, so this case is caught here
    val adjustedBase = ApfloatMath.abs(this)
    return ApfloatMath.round(ApfloatMath.pow(adjustedBase.precision(PrologBigNumber.PRECISION + 1), exponent), PrologBigNumber.PRECISION, RoundingMode.HALF_UP)
}

private fun Apfloat.divideWithFinitePrecision(divident: Apfloat): Apfloat {
    return try {
        this.divide(divident)
    } catch (ex: InfiniteExpansionException)  {
        // Apfloat implements division in terms of inverseRoot, which is
        // slightly inaccurate for large numbers
        if (this == divident) {
            return Apfloat.ONE
        }
        ApfloatMath.round(this.precision(PrologBigNumber.PRECISION + 1).divide(divident), PrologBigNumber.PRECISION, RoundingMode.HALF_UP)
    }
}