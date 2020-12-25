package com.github.prologdb.async

/**
 * If both are null, does nothing.
 * If only one is not-null, throws that.
 * If multiple are null, throws the first non-null with all other non-nulls as suppressed (see [Throwable.addSuppressed])
 *
 * If ever needed, could be extended to `throwMultipleNotNull(vararg exs: Throwable?)`.
 */
inline fun throwMultipleNotNull(ex1: Throwable?, ex2: Throwable?) {
    if (ex1 != null && ex2 != null) {
        ex1.addSuppressed(ex2)
        throw ex1
    }

    if (ex1 != null) {
        throw ex1
    }

    if (ex2 != null) {
        throw ex2
    }
}
