package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.util.crossover

private fun TermConstraint?.and(rhs: TermConstraint?): TermConstraint = when {
    this == null -> rhs ?: NoopConstraint
    rhs == null  -> this
    else -> this.and(rhs)
}

class ConstrainedTerm(
    val structure: Term,
    val constraints: Map<Variable, TermConstraint>
) {
    /**
     * Unifies the two [structure]s. If they don't unify, returns null. If they unify,
     * combines the [constraints] of both. If the constraints are incompatible, returns null.
     * @return The combined structure and constraints.
     */
    fun combineWith(rhs: ConstrainedTerm, randomVariableScope: RandomVariableScope): ConstrainedTerm? {
        val structureUnification = structure.unify(rhs.structure, randomVariableScope) ?: return null
        var combinedStructure = structure.substituteVariables(structureUnification.variableValues.asSubstitutionMapper())

        val combinedConstraints = HashMap<Variable, TermConstraint>(structureUnification.variableValues.size)
        val literalConstraints = mutableMapOf<Variable, Term>()

        for ((variable, value) in structureUnification.variableValues.values) {
            val lhsConstraint = constraints[variable]
            val rhsConstraint = rhs.constraints[variable]
            var combinedConstraint = lhsConstraint.and(rhsConstraint)

            if (value != null) {
                if (value is Variable) {
                    val lhsUnifiedConstraint = constraints[value]
                    val rhsUnifiedConstraint = rhs.constraints[value]
                    combinedConstraint = combinedConstraint
                        .and(lhsUnifiedConstraint)
                        .and(rhsUnifiedConstraint)
                } else {
                    if (!combinedConstraint.check(value)) {
                        return null
                    }
                }

                if (combinedConstraint is IdentityTermConstraint) {
                    literalConstraints[variable] = combinedConstraint.literal
                    if (value is Variable) {
                        literalConstraints[value] = combinedConstraint.literal
                    }
                } else {
                    check(combinedConstraints.putIfAbsent(variable, combinedConstraint) == null)
                    if (value is Variable) {
                        check(combinedConstraints.putIfAbsent(value, combinedConstraint) == null)
                    }
                }
            }
        }

        val combinedVariables = combinedStructure.variables

        for (variable in combinedVariables) {
            if (variable !in combinedConstraints) {
                val lhsConstraint = constraints[variable]
                val rhsConstraint = rhs.constraints[variable]
                val combinedConstraint = lhsConstraint.and(rhsConstraint)

                if (combinedConstraint is IdentityTermConstraint) {
                    literalConstraints[variable] = combinedConstraint.literal
                } else {
                    combinedConstraints[variable] = combinedConstraint
                }
            }
        }

        if (literalConstraints.isNotEmpty()) {
            combinedStructure = combinedStructure.substituteVariables { literalConstraints[it] ?: it }
            combinedConstraints.keys.removeAll(literalConstraints.keys)
        }

        val com = this.structure.variables
        combinedConstraints.keys.removeIf { it !in combinedVariables }

        return ConstrainedTerm(combinedStructure, combinedConstraints)
    }

    override fun toString(): String {
        var str = "Subject = $structure"

        val relevantConstraints = constraints.filter { (_, constraint) -> constraint != NoopConstraint }
        if (relevantConstraints.isNotEmpty()) {
            str += ", \n\t"

            str += relevantConstraints.entries.joinToString(
                transform = { (variable, constraint) -> constraint.toString(variable) },
                separator = " ,\n\t"
            )
        }

        return "$str."
    }

    companion object {
        fun unifiesWith(term: Term) = ConstrainedTerm(term, emptyMap())
        fun areMutuallyExclusive(constrainedTerms: Collection<ConstrainedTerm>): Boolean {
            if (constrainedTerms.size == 1) {
                return true
            }

            val randomVariableScope = RandomVariableScope()
            return constrainedTerms
                .crossover { a, b ->
                    val combination = a.combineWith(b, randomVariableScope)
                    combination == null || combination.constraints.values.any { it is ImpossibleConstraint }
                }
                .all { it }
        }
    }
}