package com.github.prologdb.parser

sealed class Either<out A, out B> {
    class A<out A, out B>(val value: A) : Either<A, B>() {
        operator fun component1() = value
        override fun <R> ifA(action: (A) -> R): R? = action(value)
        override fun <R> ifB(action: (B) -> R): R? = null
    }

    class B<out A, out B>(val value: B) : Either<A, B>() {
        operator fun component1() = value
        override fun <R> ifA(action: (A) -> R): R? = null
        override fun <R> ifB(action: (B) -> R): R? = action(value)
    }

    abstract fun <R> ifA(action: (A) -> R): R?
    abstract fun <R> ifB(action: (B) -> R): R?

    companion object {
        fun <A> Iterable<Either<A, *>>.filterIsA(): Iterable<A> = filterIsInstance<Either.A<A, *>>().map { it.value }
        fun <B> Iterable<Either<*, B>>.filterIsB(): Iterable<B> = filterIsInstance<Either.B<*, B>>().map { it.value }
    }
}