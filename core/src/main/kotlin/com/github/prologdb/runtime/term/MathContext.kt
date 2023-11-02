package com.github.prologdb.runtime.term

import java.math.RoundingMode as JavaRoundingMode

data class MathContext(val fractionalPrecision: Long, val roundingMode: RoundingMode) {
    enum class RoundingMode(val java: JavaRoundingMode) {
        HALF_UP(JavaRoundingMode.HALF_UP),
        HALF_DOWN(JavaRoundingMode.HALF_DOWN),
        HALF_EVEN(JavaRoundingMode.HALF_EVEN),
        UP(JavaRoundingMode.UP),
        DOWN(JavaRoundingMode.DOWN),
        CEILING(JavaRoundingMode.CEILING),
        FLOORING(JavaRoundingMode.FLOOR),
    }

    companion object {
        val DEFAULT = MathContext(30, RoundingMode.HALF_UP)
    }
}