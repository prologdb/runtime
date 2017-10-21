package com.github.tmarsteel.ktprolog.knowledge

import com.google.common.collect.BiMap
import com.github.tmarsteel.ktprolog.term.Term
import com.github.tmarsteel.ktprolog.term.Variable

/**
 * Handles the remapping of user-specified variables to random variables (that avoids name collisions).
 */
class RandomVariableScope {
    /**
     * This is essentially the most simple PRNG possible: this int will just be incremented until it hits Int.MAX_VALUE
     * and will then throw an exception. That number is then used to generate variable names.
     */
    private var randomCounter = 0

    /**
     * Replaces all the variables in the given predicate with random instances; the mapping gets stored
     * in the given map.
     */
    fun withRandomVariables(term: Term, mappings: BiMap<Variable, Variable>): Term {
        return term.substituteVariables { it ->
            if (it !in mappings.inverse()) {
                val randomVariable = createNewRandomVariable()
                mappings[randomVariable] = it
            }
            mappings.inverse()[it]!!
        }
    }

    fun createNewRandomVariable(): Variable {
        return RandomVariable(randomCounter++)
    }
}

class RandomVariable(private val counter: Int) : Variable("_G" + counter)