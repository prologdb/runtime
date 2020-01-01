package com.github.prologdb.runtime.util

fun <T, R : Any> Iterable<T>.crossover(compute: (T, T) -> R): Iterable<R> = object : Iterable<R> {
    override fun iterator(): Iterator<R> {
        val allSourceElements = this@crossover.toRandomAccessList()

        if (allSourceElements.size < 2) {
            throw IllegalArgumentException("Can only compute a crossover for n >= 2 elements")
        }

        return object : Iterator<R> {
            var currentFirstRoundIndex: Int = 0
            var currentSecondRoundIndex: Int = 1

            var next: R? = null

            override fun hasNext(): Boolean {
                if (next == null) {
                    findNext()
                }

                return next != null
            }

            override fun next(): R {
                if (!hasNext()) {
                    throw NoSuchElementException()
                }

                val ret = next!!
                next = null
                return ret
            }

            private fun findNext() {
                if (currentSecondRoundIndex > allSourceElements.lastIndex) {
                    currentFirstRoundIndex++
                    currentSecondRoundIndex = currentFirstRoundIndex + 1
                }

                if (currentSecondRoundIndex > allSourceElements.lastIndex) {
                    return
                }

                val a = allSourceElements[currentFirstRoundIndex]
                val b = allSourceElements[currentSecondRoundIndex]
                next = compute(a, b)

                currentSecondRoundIndex++
            }
        }
    }
}

fun <T> Iterable<T>.toRandomAccessList(): List<T> {
    return if (this is List && this is RandomAccess) {
        this
    } else if (this is Collection) {
        toCollection(ArrayList(size))
    } else {
        toCollection(ArrayList())
    }
}

fun <T> Iterable<T>.allIndexed(predicate: (Int, T) -> Boolean): Boolean {
    if (this is Collection && this.isEmpty()) return true
    this.forEachIndexed { index, element ->
        if (!predicate(index, element)) return@allIndexed false
    }
    return true
}

fun <K : Any, V : Any> Iterable<Map<K, V>>.toMultiValueMap(): Map<K, List<V>> {
    if (this is Collection && this.isEmpty()) return emptyMap()
    val iterator = iterator()
    val first = iterator.next()
    val result = HashMap<K, MutableList<V>>(first.size)
    first.forEach { (key, value) -> result[key] = mutableListOf(value) }
    while (iterator.hasNext()) {
        val current = iterator.next()
        current.forEach { (key, value) ->
            result.computeIfAbsent(key, { mutableListOf() }).add(value)
        }
    }

    return result
}

fun <K : Any, V: Any, M> Map<K, V>.mapMapValues(mapper: (V) -> M): Map<K, M> {
    val result = HashMap<K, M>(size)
    forEach { key, value -> result[key] = mapper(value) }
    return result
}

fun <T, M : Any> Iterable<T>.associateWithNotNull(mapper: (T) -> M?): Map<T, M> {
    return asSequence()
        .mapNotNull { mapper(it)?.let { mapped -> it to mapped } }
        .toMap()
}

fun <T, V> permutateMap(first: Iterable<T>, second: Iterable<T>, action: (T, T) -> V): Sequence<V> = sequence {
    for (eFromFirst in first) {
        for (eFromSecond in second) {
            yield(action(eFromFirst, eFromSecond))
        }
    }
}