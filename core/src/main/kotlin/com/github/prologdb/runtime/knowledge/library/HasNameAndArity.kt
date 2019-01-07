package com.github.prologdb.runtime.knowledge.library

/**
 * Something with a functor and an arity (for the scope of this runtime: compound terms and predicate indicators)
 */
interface HasNameAndArity {
    val name: String
    val arity: Int
}

