package com.github.prologdb.runtime.term

import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.PrologSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.unification.Unification
import org.apfloat.Apfloat
import org.apfloat.ApfloatMath
import org.apfloat.Apint
import org.apfloat.ApintMath
import org.apfloat.InfiniteExpansionException
import java.io.Writer
import java.math.BigInteger
import java.util.concurrent.ConcurrentHashMap
import kotlin.math.absoluteValue
import kotlin.math.max

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

    abstract fun plus(other: PrologNumber, context: MathContext): PrologNumber
    abstract fun minus(other: PrologNumber, context: MathContext): PrologNumber
    abstract fun times(other: PrologNumber, context: MathContext): PrologNumber
    abstract fun div(other: PrologNumber, context: MathContext): PrologNumber
    abstract fun rem(other: PrologNumber, context: MathContext): PrologNumber
    abstract fun toThe(other: PrologNumber, context: MathContext): PrologNumber
    abstract operator fun compareTo(other: PrologNumber) : Int

    abstract operator fun unaryPlus(): PrologNumber
    abstract operator fun unaryMinus(): PrologNumber

    abstract fun ceil(): PrologNumber
    abstract fun floor(): PrologNumber

    abstract fun sqrt(context: MathContext): PrologNumber

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
        private val LONG_MAX_APFLOAT = Apfloat(Long.MAX_VALUE)

        /* The integers 0 through 65536 are cached: these are used for
           strings and this avoids a lot of memory overhead. Also, this
           likely applies to other uses-cases where small numbers are used.
         */
        private val CACHE: MutableMap<UShort, PrologNumber> = ConcurrentHashMap()
        private val CACHEABLE_RANGE_INT = IntRange(0, UShort.MAX_VALUE.toInt())
        private val CACHEABLE_RANGE_UINT = UIntRange(0.toUInt(), UShort.MAX_VALUE.toUInt())
        private val CACHEABLE_RANGE_LONG = LongRange(0.toLong(), UShort.MAX_VALUE.toLong())
        private val CACHEABLE_RANGE_ULONG = ULongRange(0.toULong(), UShort.MAX_VALUE.toULong())

        operator fun invoke(value: Byte): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: UByte): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: Short): PrologNumber = PrologLongInteger(value.toLong())
        operator fun invoke(value: UShort): PrologNumber {
            return CACHE.getOrPut(value) { PrologLongInteger(value.toLong()) }
        }
        operator fun invoke(value: Int): PrologNumber = if (value in CACHEABLE_RANGE_INT) PrologNumber(value.toUShort()) else PrologLongInteger(value.toLong())
        operator fun invoke(value: UInt): PrologNumber = if (value in CACHEABLE_RANGE_UINT) PrologNumber(value.toUShort()) else PrologLongInteger(value.toLong())
        operator fun invoke(value: Long): PrologNumber = if (value in CACHEABLE_RANGE_LONG) PrologNumber(value.toUShort()) else PrologLongInteger(value)
        operator fun invoke(value: ULong): PrologNumber {
            if (value in CACHEABLE_RANGE_ULONG) {
                return PrologNumber(value.toUShort())
            }

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

@PrologTypeName("number")
class PrologLongInteger(
    val value: Long
) : PrologNumber() {
    override val isInteger = true
    override fun plus(other: PrologNumber, context: MathContext) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, context, Math::addExact, Apfloat::add)
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).add(other.value))
        }

    override fun minus(other: PrologNumber, context: MathContext) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, context, Math::subtractExact, Apfloat::subtract)
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).subtract(other.value))
        }

    override fun times(other: PrologNumber, context: MathContext) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, context, Math::multiplyExact, Apfloat::multiply)
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).multiply(other.value))
        }

    override fun div(other: PrologNumber, context: MathContext) =
        when(other) {
            is PrologLongInteger -> this.value.combineExact(other.value, context, ::divideExact) { divident, divisor -> divident.divideWithFinitePrecision(divisor, context) }
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).divideWithFinitePrecision(other.value, context))
        }

    override fun rem(other: PrologNumber, context: MathContext) =
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

    override fun toThe(other: PrologNumber, context: MathContext): PrologNumber {
        return when(other) {
            is PrologLongInteger -> PrologBigNumber(Apfloat(this.value).powSupportingNegativeBase(Apfloat(other.value), context))
            is PrologBigNumber -> PrologBigNumber(Apfloat(this.value).powSupportingNegativeBase(other.value, context))
        }
    }

    override fun ceil() = this
    override fun floor() = this
    override fun unaryPlus() = if (this.value >= 0) this else PrologLongInteger(+this.value)
    override fun unaryMinus() = PrologLongInteger(-this.value)
    override fun toInteger(): Long = value
    override fun toDecimal(): Double = value.toDouble()
    override fun sqrt(context: MathContext) = PrologBigNumber(context.transformWithCappedPrecision(Apfloat(this.value), ApfloatMath::sqrt))

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
    constructor(data: ByteArray, off: Int, len: Int, signum: Int, scale: Long = 1) : this({
        val bigInt = BigInteger(signum, data, off, len)
        val unscaled = Apfloat(bigInt, Apfloat.INFINITE, 10)
        ApfloatMath.scale(unscaled, scale)
    }())

    override fun plus(other: PrologNumber, context: MathContext) = combineSimple(other, Apfloat::add)
    override fun minus(other: PrologNumber, context: MathContext) = combineSimple(other, Apfloat::subtract)
    override fun times(other: PrologNumber, context: MathContext) = combineSimple(other, Apfloat::multiply)
    override fun div(other: PrologNumber, context: MathContext) = combineSimple(other) { divident, divisor -> divident.divideWithFinitePrecision(divisor, context) }
    override fun rem(other: PrologNumber, context: MathContext) = combineSimple(other, Apfloat::mod)
    override fun toThe(other: PrologNumber, context: MathContext) = combineSimple(other) { base, exp -> base.powSupportingNegativeBase(exp, context) }
    override fun compareTo(other: PrologNumber) = when (other) {
        is PrologLongInteger -> this.value.compareTo(Apfloat(other.value))
        is PrologBigNumber -> this.value.compareTo(other.value)
    }
    override fun unaryPlus() = if (this.value.signum() >= 0) this else PrologBigNumber(this.value.negate())
    override fun unaryMinus() = PrologBigNumber(this.value.negate())
    override fun ceil() = PrologBigNumber(this.value.ceil())
    override fun floor() = PrologBigNumber(this.value.floor())
    override fun sqrt(context: MathContext): PrologNumber = PrologBigNumber(context.transformWithCappedPrecision(this.value, ApfloatMath::sqrt))
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
        if (value == Apfloat.ZERO) {
            return Triple(byteArrayOf(), 0, 0)
        }

        val significantDigitsAsInt = ApintMath.abs(ApfloatMath.scale(value, -value.scale() + value.size()).toRadix(16).truncate())
        return Triple(significantDigitsAsInt.toByteArray(), value.signum(), value.scale() - value.size())
    }

    private inline fun combineSimple(other: PrologNumber, crossinline operation: (Apfloat, Apfloat) -> Apfloat): PrologNumber {
        return when (other) {
            is PrologLongInteger -> PrologBigNumber(operation(this.value, Apfloat(other.value)))
            is PrologBigNumber -> PrologBigNumber(operation(this.value, other.value))
        }
    }
}

private fun divideExact(divident: Long, divisor: Long): Long {
    if (divident % divisor != 0L) {
        throw ArithmeticException("Integer division is not accurate")
    }

    return divident / divisor
}

private fun Long.toApfloatWithFinitePrecision(context: MathContext): Apfloat {
    val uncapped = Apfloat(this)
    return uncapped.precision(max(uncapped.scale().absoluteValue, context.fractionalPrecision + 1))
}

/**
 * @param simpleCombinator must throw [ArithmeticException] if the operation is not exact. Use method references from [Math],
 * e.g. [Math.addExact]
 */
private inline fun Long.combineExact(
    other: Long,
    context: MathContext,
    crossinline simpleCombinator: (Long, Long) -> Long,
    crossinline apfloatCombinator: (Apfloat, Apfloat) -> Apfloat,
): PrologNumber {
    return try {
        PrologLongInteger(simpleCombinator(this, other))
    }
    catch (_: ArithmeticException) {
        val rawResult = apfloatCombinator(this.toApfloatWithFinitePrecision(context), other.toApfloatWithFinitePrecision(context))
        PrologBigNumber(rawResult.roundToFractionalPrecision(context))
    }
}

private fun Apfloat.roundToFractionalPrecision(context: MathContext): Apfloat {
    val precision = this.precision()
    if (precision == Apfloat.INFINITE) {
        return ApfloatMath.round(this, precision, context.roundingMode.java)
    }

    val fractionalPrecision = (this.precision() - this.scale().absoluteValue).absoluteValue
    return if (fractionalPrecision > context.fractionalPrecision) {
        ApfloatMath.round(this, this.scale().absoluteValue + context.fractionalPrecision, context.roundingMode.java)
    } else {
        this
    }
}

private fun Apfloat.fractionalPrecision(fracPrecision: Long): Apfloat = precision(scale().absoluteValue + fracPrecision)

private inline fun MathContext.transformWithCappedPrecision(value: Apfloat, crossinline transform: (Apfloat) -> Apfloat): Apfloat {
    return try {
        transform(value)
    } catch (_: InfiniteExpansionException) {
        transform(value.fractionalPrecision(fractionalPrecision + 1)).roundToFractionalPrecision(this)
    }
}

private fun Apfloat.powSupportingNegativeBase(exponent: Apfloat, context: MathContext): Apfloat {
    // ApfloatMath rejects the operation if the base is negative as the result would be a complex number
    // however, SWI, GNU prolog, Tau prolog and my SHARP calculator agree that `0.25 is (-0.5)^2`, so this case is caught here
    val adjustedBase = ApfloatMath.abs(this)

    val rawResult = try {
        ApfloatMath.pow(adjustedBase, exponent)
    } catch (_: InfiniteExpansionException) {
        ApfloatMath.pow(adjustedBase.fractionalPrecision(context.fractionalPrecision), exponent)
    }

    return if (adjustedBase.isInteger && exponent.isInteger && exponent.signum() >= 0) {
        // result has to be integer as well
        ApfloatMath.round(rawResult, rawResult.scale().absoluteValue, context.roundingMode.java)
    } else {
        rawResult.roundToFractionalPrecision(context)
    }
}

private fun Apfloat.divideWithFinitePrecision(divident: Apfloat, context: MathContext): Apfloat {
    return try {
        this.divide(divident)
    } catch (ex: InfiniteExpansionException)  {
        // Apfloat implements division in terms of inverseRoot, which is
        // slightly inaccurate for large numbers
        if (this == divident) {
            return Apfloat.ONE
        }

        this
            .precision(this.scale().absoluteValue + context.fractionalPrecision + 1)
            .divide(divident)
            .roundToFractionalPrecision(context)
    }
}

/**
 * equivalent to [ApfloatHelper.toBigInteger] + [BigInteger.toByteArray], but without copying the byte array around.
 */
private fun Apint.toByteArray(): ByteArray {
    check(this.radix() == 16)

    val byteCount = (scale() + 1 shr 1)
    if (byteCount > Int.MAX_VALUE) {
        throw IllegalArgumentException("Cannot represent a number of scale ${scale()} as a byte-array; exceeds maximum array size of ${Int.MAX_VALUE}")
    }

    val startOnHighHalfByte = scale() and 1L == 0L

    val data = ByteArray(byteCount.toInt())
    writeTo(object : Writer() {
        override fun close() {
        }

        override fun flush() {
        }

        override fun write(cbuf: CharArray, off: Int, len: Int) {
            for (i in off..(off + len)) {
                write(cbuf[i].code)
            }
        }

        private var isHighHalfByte = startOnHighHalfByte
        private var carry: Int = 0
        private var bytePosition = 0

        override fun write(c: Int) {
            val value = Character.digit(c, 16)
            if (isHighHalfByte) {
                carry = value shl 4
            } else {
                carry += (value and 0x0F)
                data[bytePosition++] = carry.toByte()
            }

            isHighHalfByte = !isHighHalfByte
        }
    })

    return data
}