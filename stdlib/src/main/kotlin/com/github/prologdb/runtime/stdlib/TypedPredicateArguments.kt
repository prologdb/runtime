package com.github.prologdb.runtime.stdlib

import com.github.prologdb.runtime.ArgumentTypeError
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.term.Term

class TypedPredicateArguments(val indicator: ClauseIndicator, val raw: Array<out Term>) {
    val size = raw.size
    operator fun get(index: Int): Term = raw[index]

    fun <T : Term> getTyped(index: Int, type: Class<T>): T {
        val untyped = this[index]
        if (type.isInstance(untyped)) {
            @Suppress("unchecked_cast")
            return untyped as T
        } else {
            throw ArgumentTypeError(indicator, index, untyped, type)
        }
    }

    inline fun <reified T : Term> getTyped(index: Int) = getTyped(index, T::class.java)
}
