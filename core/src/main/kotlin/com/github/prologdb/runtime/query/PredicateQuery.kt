package com.github.prologdb.runtime.query

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

open class PredicateQuery(val predicate: Predicate) : Query {
    override fun findProofWithin(kb: KnowledgeBase, initialVariables: VariableBucket, randomVarsScope: RandomVariableScope): LazySequence<Unification> {
        val substitutedPredicate = predicate.substituteVariables(initialVariables.asSubstitutionMapper())

        return kb.fulfill(substitutedPredicate, randomVarsScope)
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return PredicateQuery(
            randomVarsScope.withRandomVariables(predicate, mapping) as Predicate
        )
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return PredicateQuery(
            predicate.substituteVariables(variableValues.asSubstitutionMapper())
        )
    }

    override fun toString(): String {
        return predicate.toString()
    }
}