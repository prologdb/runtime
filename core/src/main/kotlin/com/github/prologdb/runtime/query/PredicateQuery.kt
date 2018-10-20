package com.github.prologdb.runtime.query

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.*
import com.github.prologdb.runtime.builtin.getInvocationStackFrame
import com.github.prologdb.runtime.builtin.prologSourceInformation
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket

open class PredicateQuery(
    val predicate: Predicate,
    override val sourceInformation: PrologSourceInformation
) : Query, HasPrologSource
{
    constructor(predicate: Predicate) : this(predicate, getInvocationStackFrame().prologSourceInformation)

    private val stackFrame: PrologStackTraceElement by lazy {
        PrologStackTraceElement(
            predicate,
            sourceInformation
        )
    }

    override val findProofWithin: suspend LazySequenceBuilder<Unification>.(ProofSearchContext, VariableBucket) -> Unit = { context, initialVariables ->
        val substitutedPredicate = predicate.substituteVariables(initialVariables.asSubstitutionMapper())

        yieldAll(
            context.knowledgeBase.fulfill(substitutedPredicate, context)
                .amendExceptionsWithStackTraceOnRemaining(stackFrame)
        )
    }

    override fun withRandomVariables(randomVarsScope: RandomVariableScope, mapping: VariableMapping): Query {
        return PredicateQuery(
            randomVarsScope.withRandomVariables(predicate, mapping) as Predicate,
            sourceInformation
        )
    }

    override fun substituteVariables(variableValues: VariableBucket): Query {
        return PredicateQuery(
            predicate.substituteVariables(variableValues.asSubstitutionMapper()),
            sourceInformation
        )
    }

    override fun toString(): String {
        return predicate.toString()
    }
}