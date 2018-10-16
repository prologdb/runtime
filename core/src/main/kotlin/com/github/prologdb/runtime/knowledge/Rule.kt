package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.async.LazySequence
import com.github.prologdb.runtime.async.mapRemaining
import com.github.prologdb.runtime.knowledge.library.LibraryEntry
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

open class Rule(val head: Predicate, val query: Query) : LibraryEntry {
    override val name = head.name
    override val arity = head.arity

    open fun fulfill(predicate: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        val predicateRandomVarsMapping = VariableMapping()
        val randomPredicate = randomVariableScope.withRandomVariables(predicate, predicateRandomVarsMapping)

        val ruleRandomVarsMapping = VariableMapping()
        val randomHead = randomVariableScope.withRandomVariables(head, ruleRandomVarsMapping)

        val predicateAndHeadUnification = randomHead.unify(randomPredicate)
        if (predicateAndHeadUnification == null) {
            // this rule cannot be used to fulfill the given predicate
            return Unification.NONE
        }

        val randomQuery = query
            .withRandomVariables(randomVariableScope, ruleRandomVarsMapping)
            .substituteVariables(predicateAndHeadUnification.variableValues)

        return randomQuery.findProofWithin(kb, VariableBucket(), randomVariableScope)
            .mapRemaining { unification ->
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
            }
    }

    override fun unifyWithKnowledge(other: Predicate, kb: KnowledgeBase, randomVariableScope: RandomVariableScope): LazySequence<Unification> {
        return fulfill(other, kb, randomVariableScope)
    }

    override fun toString() = "$head :- $query"
}