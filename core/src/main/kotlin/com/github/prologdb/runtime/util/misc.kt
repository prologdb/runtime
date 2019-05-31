package com.github.prologdb.runtime.util

public fun <T, R> Iterable<out T>.crossover(compute: (T, T) -> R): Iterable<R> = object : Iterable<R> {
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
                next = compute(a, b) ?: throw RuntimeException("Illegal compute function: returned null")

                currentSecondRoundIndex++
            }
        }
    }
}

public fun <T> Iterable<T>.toRandomAccessList(): List<T> {
    val destination = if (this is Collection) ArrayList<T>(this.size) else ArrayList()
    return this.toCollection(destination)
}
