package com.github.prologdb.runtime.proofsearch

import com.github.prologdb.runtime.term.Term

class CurryingCallable(
    private val baseCallable: PrologCallable,
    private val prependArguments: Array<out Term>,
) : PrologCallable {
    override val functor: String = baseCallable.functor
    override val arity: Int = baseCallable.arity + prependArguments.size
    override val fulfill: PrologCallableFulfill = { args, ctxt ->
        baseCallable.fulfill(this, prependArguments + args, ctxt)
    }
}

private inline operator fun <reified T> Array<out T>.plus(other: Array<out T>): Array<out T> {
    return Array(this.size + other.size) { fullIndex ->
        if (fullIndex <= this.lastIndex) this[fullIndex] else other[fullIndex - this.size]
    }
}