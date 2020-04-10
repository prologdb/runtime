package com.github.prologdb.runtime.analyzation.constraint

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.term.Variable
import com.github.prologdb.runtime.util.crossover
import unify

private fun TermConstraint?.and(rhs: TermConstraint?, randomVariableScope: RandomVariableScope): TermConstraint? = when {
    this == null && rhs == null -> null
    this == null -> rhs ?: NoopConstraint
    rhs == null  -> this
    else         -> this.and(rhs, randomVariableScope)
}

open class GoalBehaviour(
    /**
     * If these constraints are met, the effects described in [outConstraints] are to be expected.
     */
    val inConstraints: Map<Variable, TermConstraint>,

    /**
     * Every element in this list resembles a solution the goal emits. For every solution, the effects on the
     * out-variables are defined in the associated [TermConstraints]
     */
    val outConstraints: List<Map<Variable, TermConstraint>>
)

class InvocationBehaviour(
    givenHead: CompoundTerm,
    inConstraints: Map<Variable, TermConstraint>,
    outConstraints: List<Map<Variable, TermConstraint>>
) : GoalBehaviour(inConstraints, outConstraints) {

    val targetCallableHead: CompoundTerm = givenHead.substituteVariables { variable ->
        val unificationConstraint = (inConstraints[variable] as? UnificationTermConstraint)
        if (unificationConstraint?.negated == false) unificationConstraint.unifiesWith else variable
    }

    fun isMutuallyExclusiveWith(rhs: InvocationBehaviour, randomVariableScope: RandomVariableScope): Boolean {
        TODO()
        val structureUnification = targetCallableHead.unify(rhs.targetCallableHead, randomVariableScope) ?: return true

        for ((variable, value) in structureUnification.variableValues.values) {
            val lhsConstraint = inConstraints[variable]
            val rhsConstraint = rhs.inConstraints[variable]
            var combinedConstraint = lhsConstraint.and(rhsConstraint, randomVariableScope)

            if (value != null) {
                if (value is Variable) {
                    val lhsUnifiedConstraint = inConstraints[value]
                    val rhsUnifiedConstraint = rhs.inConstraints[value]
                    combinedConstraint = combinedConstraint
                        .and(lhsUnifiedConstraint, randomVariableScope)
                        .and(rhsUnifiedConstraint, randomVariableScope)
                } else {
                    if (combinedConstraint?.check(value) == false) {
                        return true
                    }
                }

                if (combinedConstraint == NoopConstraint) {
                    return true
                }
            }
        }

        return false
    }

    /**
     * To be used when the structure should be the given [Term] instead of the current one (e.g. when
     * a predicate is invoked you'll want to exchange the structure taken from the clause declaration with
     * the actual invocation term).
     *
     * @return If the given term can be used as the structure, a [InvocationBehaviour] with a structure as close to
     * the given term as possible and adjusted constraints. It is guaranteed that the variable scopes of this [InvocationBehaviour]s
     * [targetCallableHead] and that of the given term are not conflated. Returns null if the given term and this [InvocationBehaviour]s
     * structure do not unify.
     */
    fun translate(invocation: CompoundTerm, randomVariableScope: RandomVariableScope): InvocationBehaviour? {
        if (targetCallableHead.arity != invocation.arity) {
            return null
        }

        val (selfRandom, selfMapping) = withRandomVariables(randomVariableScope)
        val rhsMapping = VariableMapping()
        val rhsRandom = randomVariableScope.withRandomVariables(invocation, rhsMapping)

        val invocationUnification = selfRandom.targetCallableHead.arguments.unify(rhsRandom.arguments, randomVariableScope) ?: return null

        for ((variable, term) in invocationUnification.variableValues.values) {
            term!!
            if (term !is Variable && selfMapping.hasSubstitution(variable)) {
                val unrandomized = selfMapping.getOriginal(variable)!!
                val constraint = inConstraints[unrandomized]
                if (constraint?.check(term) == false) {
                    return null
                }
            }
        }

        fun substitutor(variable: Variable): Term {
            return if (invocationUnification.variableValues.contains(variable)) {
                invocationUnification.variableValues[variable].substituteVariables(::substitutor)
            } else if (selfMapping.hasSubstitution(variable)) {
                val unrandomized = selfMapping.getOriginal(variable)!!
                (inConstraints[unrandomized] as? IdentityTermConstraint)?.literal ?: variable
            } else if (rhsMapping.hasSubstitution(variable)) {
                rhsMapping.getOriginal(variable)!!
            } else {
                variable
            }
        }

        val resultInvocation = rhsRandom.substituteVariables(::substitutor)

        fun Map<Variable, TermConstraint>.translate() = mapNotNull { (variable, constraint) ->
            if (constraint is IdentityTermConstraint) return@mapNotNull null // inlined before
            val randomizedVariable = selfMapping.getSubstitution(variable) ?: return@mapNotNull null

            val resultVar = if (invocationUnification.variableValues.contains(randomizedVariable)) {
                rhsMapping.getOriginal(invocationUnification.variableValues[randomizedVariable] as Variable)!!
            } else {
                randomizedVariable
            }

            resultVar to constraint
        }
            .toMap()

        return InvocationBehaviour(resultInvocation, inConstraints.translate(), outConstraints.map { it.translate() })
    }

    /**
     * @return first: a [InvocationBehaviour] with this' structure transformed by [RandomVariableScope.withRandomVariables]
     * and an adjusted constraint map. Second: the [VariableMapping] obtained from [RandomVariableScope.withRandomVariables]:
     * maps the original variables of this' [targetCallableHead] to those of the returned [InvocationBehaviour].
     */
    fun withRandomVariables(randomVariableScope: RandomVariableScope): Pair<InvocationBehaviour, VariableMapping> {
        val mapping = VariableMapping()
        val randomStructure = randomVariableScope.withRandomVariables(targetCallableHead, mapping)
        val randomInConstraints = inConstraints
            .map { (variable, constraint) -> mapping.getSubstitution(variable)!! to constraint }
            .toMap()
        val randomOutConstraints = outConstraints
            .map { outConstraint ->
                outConstraint.map { (variable, constraint) -> mapping.getSubstitution(variable)!! to constraint }
                    .toMap()
            }

        return InvocationBehaviour(randomStructure, randomInConstraints, randomOutConstraints) to mapping
    }

    override fun toString(): String {
        val str = StringBuilder(50)

        inConstraints
            .filter { (_, constraint) -> constraint != NoopConstraint }
            .forEach { (variable, constraint) ->
                str.append(constraint.toString(variable))
                str.append(",\n")
            }

        str.append(targetCallableHead)

        val relevantOutConstraints = outConstraints
            .filter { it.isNotEmpty() }
            .filter { solution -> solution.values.none { it is ImpossibleConstraint } }

        if (relevantOutConstraints.isNotEmpty()) {
            str.append(relevantOutConstraints.joinToString(
                prefix = ",\n((\n\t",
                transform = {
                    it.entries
                        .filter { (_, constraint) -> constraint !is ImpossibleConstraint }
                        .joinToString(
                            transform = { (variable, constraint) -> constraint.toString(variable) },
                            separator = ",\n\t"
                        )
                },
                separator = "\n) ; (\n\t",
                postfix = "\n))"
            ))
        }
        else if (outConstraints.isEmpty()) {
            str.append(" ,\n\tfalse");
        }

        str.append(".")
        return str.toString()
    }

    companion object {
        fun unifiesWith(term: CompoundTerm) = InvocationBehaviour(term, emptyMap(), listOf(emptyMap()))

        fun areMutuallyExclusive(constrainedTerms: Collection<InvocationBehaviour>): Boolean {
            if (constrainedTerms.size < 2) {
                return true
            }

            val randomVariableScope = RandomVariableScope()
            return constrainedTerms
                .crossover { a, b -> a.isMutuallyExclusiveWith(b, randomVariableScope) }
                .all { it }
        }

        fun combine(
            randomVariableScope: RandomVariableScope,
            left: Map<Variable, TermConstraint>,
            vararg right: Map<Variable, TermConstraint>
        ): Map<Variable, TermConstraint>? {
            if (right.isEmpty()) {
                return left
            }

            val result = HashMap<Variable, TermConstraint>((left.size * 3) / 2)
            result.putAll(left)

            for (map in right) {
                for ((rightVariable, rightConstraint) in map) {
                    if (rightVariable in result) {
                        val combined = result.getValue(rightVariable).and(rightConstraint, randomVariableScope)
                        if (combined is ImpossibleConstraint) {
                            return null
                        }
                        result[rightVariable] = combined
                    }
                    else {
                        result[rightVariable] = rightConstraint
                    }
                }
            }

            return result
        }
    }
}