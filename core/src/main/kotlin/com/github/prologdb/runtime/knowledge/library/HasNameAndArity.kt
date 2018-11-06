package com.github.prologdb.runtime.knowledge.library

/**
 * An indicator of a predicate, e.g. `likes/2`.
 */
interface HasNameAndArity {
    val name: String
    val arity: Int
}

