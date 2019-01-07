package com.github.prologdb.runtime.knowledge.library

/**
 * Something with a functor and an arity (for the scope of this runtime: compound terms and predicate indicators)
 */
interface HasFunctorAndArity {
    val functor: String
    val arity: Int
}

