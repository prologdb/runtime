package com.github.prologdb.async

/**
 * Overrides [equals] and [hashCode] by delegating to [key].
 */
class KeyIdentity<Value, Key : Any>(val value: Value, val key: Key) {
    override fun equals(other: Any?): Boolean {
        if (other !is KeyIdentity<*, *>) {
            return false
        }

        return this.key == other.key
    }

    override fun hashCode() = key.hashCode()

    override fun toString() = "KeyIdentity[key = $key, value = $value]"

    operator fun component1() = value
    operator fun component2() = key
}