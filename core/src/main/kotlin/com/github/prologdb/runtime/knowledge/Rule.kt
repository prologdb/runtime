package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.mapRemaining
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.library.Clause
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

open class Rule(val head: Predicate, val query: Query) : Clause {
    override val name = head.name
    override val arity = head.arity

    open val fulfill: suspend LazySequenceBuilder<Unification>.(predicate: Predicate, context: ProofSearchContext) -> Unit = { predicate, context ->
        val predicateRandomVarsMapping = VariableMapping()
        val randomPredicate = context.randomVariableScope.withRandomVariables(predicate, predicateRandomVarsMapping)

        val ruleRandomVarsMapping = VariableMapping()
        val randomHead = context.randomVariableScope.withRandomVariables(head, ruleRandomVarsMapping)

        val predicateAndHeadUnification = randomHead.unify(randomPredicate)
        if (predicateAndHeadUnification != null) {
            val randomQuery = query
                .withRandomVariables(context.randomVariableScope, ruleRandomVarsMapping)
                .substituteVariables(predicateAndHeadUnification.variableValues)

            val randomResults = buildLazySequence<Unification>(context.principal) {
                context.fulfillAttach(this, randomQuery, VariableBucket())
            }

            yieldAll(randomResults.mapRemaining { unification ->
                val solutionVars = VariableBucket()

                for (randomPredicateVariable in randomPredicate.variables)
                {
                    if (predicateAndHeadUnification.variableValues.isInstantiated(randomPredicateVariable)) {
                        val value = predicateAndHeadUnification.variableValues[randomPredicateVariable]
                            .substituteVariables(unification.variableValues.asSubstitutionMapper())
                            .substituteVariables(predicateAndHeadUnification.variableValues.asSubstitutionMapper())

                        solutionVars.instantiate(randomPredicateVariable, value)
                    }
                    else if (unification.variableValues.isInstantiated(randomPredicateVariable)) {
                        val originalVar = predicateRandomVarsMapping.getOriginal(randomPredicateVariable)!!
                        solutionVars.instantiate(originalVar, unification.variableValues[randomPredicateVariable])
                    }
                }

                Unification(solutionVars
                    .withVariablesResolvedFrom(predicateRandomVarsMapping))
            })
        }
        // else: does not match the rule head
    }

    override val unifyWithKnowledge: suspend LazySequenceBuilder<Unification>.(other: Predicate, context: ProofSearchContext) -> Unit
        get() = fulfill

    override fun toString() = "$head :- $query"
}