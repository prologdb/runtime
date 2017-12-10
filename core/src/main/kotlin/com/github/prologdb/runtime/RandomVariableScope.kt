package com.github.prologdb.runtime

import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable

/**
 * Handles the remapping of user-specified variables to random variables (that avoids name collisions).
 */
class RandomVariableScope {
    /**
     * This is essentially the most simple PRNG possible: this int will just be incremented until it hits Long.MAX_VALUE
     * and will then throw an exception. That number is then used to generate variable names.
     */
    private var randomCounter: Long = 0

    /**
     * Stores created instances of [RandomVariable] for reuse. Index within the list equals the counter values
     * used to create the variable; thus, this holds only the random variables created for random counter
     * values up to [Integer.MAX_VALUE]
     */
    private val variables: MutableList<RandomVariable> = ArrayList(30)

    /**
     * Replaces all the variables in the given predicate with random instances; the mapping gets stored
     * in the given map.
     */
    fun withRandomVariables(term: Term, mapping: VariableMapping): Term {
        return term.substituteVariables { originalVariable ->
            if (originalVariable == Variable.ANONYMOUS) {
                createNewRandomVariable()
            }
            else {
                if (!mapping.hasOriginal(originalVariable)) {
                    val randomVariable = createNewRandomVariable()
                    mapping.storeSubstitution(originalVariable, randomVariable)
                }

                mapping.getSubstitution(originalVariable)!!
            }
        }
    }

    fun createNewRandomVariable(): Variable {
        if (randomCounter == Long.MAX_VALUE) {
            throw PrologRuntimeException("Out of random variables")
        }

        if (variables.size > randomCounter) {
            return variables[(randomCounter++).toInt()]
        }
        else {
            val rvar = RandomVariable(randomCounter++)
            variables.add(rvar)
            return rvar
        }
    }
}
