package com.github.prologdb.runtime.analyzation.constraint

/**
 * Behaviour of a predicate when invoked (given certain conditions on the input, see [TermConstraint]).
 */
enum class DeterminismLevel {
    /**
     * The predicate yields exactly one solution.
     */
    DETERMINISTIC,

    /**
     * The predicate yields at most one solution.
     */
    SEMI_DETERMINISTIC,

    /**
     * The predicate yields zero, one or multiple solutions
     */
    NON_DETERMINISTIC,

    /**
     * The predicate yields zero, one or multiple solutions and then fails/raises an exception.
     */
    FAILING
}