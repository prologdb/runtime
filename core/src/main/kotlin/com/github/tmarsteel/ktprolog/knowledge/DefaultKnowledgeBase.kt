package com.github.tmarsteel.ktprolog.knowledge

import com.google.common.collect.HashBiMap
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Variable
import com.github.tmarsteel.ktprolog.unification.Unification
import com.github.tmarsteel.ktprolog.unification.UnsupportedExpressionException
import com.github.tmarsteel.ktprolog.unification.VariableBucket
import kotlin.coroutines.experimental.buildSequence

class DefaultKnowledgeBase : MutableKnowledgeBase {
    private val predicates: MutableSet<Predicate> = HashSet()
    private val rules: MutableSet<Rule> = HashSet() // unused as of now

    override fun fulfill(predicate: Predicate): Sequence<Unification> {
        var randomVarScope = RandomVariableScope()

        // replace all variables in the term with random ones to prevent name collisions
        val termMappings = HashBiMap.create<Variable, Variable>()
        val replaced = randomVarScope.withRandomVariables(predicate, termMappings)

        return buildSequence<Unification> {
            for (knownPredicate in predicates) {
                val knownPredicateReplaced = randomVarScope.withRandomVariables(knownPredicate, HashBiMap.create<Variable, Variable>())
                val unification = knownPredicateReplaced.unify(replaced)
                if (unification != null) {
                    val resolvedBucket = unification.variableValues.withVariablesResolvedFrom(termMappings)
                    resolvedBucket.retainAll(predicate.variables)
                    yield(Unification(resolvedBucket))
                }
            }

            for (knownRule in rules) {
                yieldAll(knownRule.fulfill(predicate, this@DefaultKnowledgeBase, randomVarScope))
            }
        }
    }

    override fun assert(predicate: Predicate) {
        if (predicate is Predicate) {
            predicates.add(predicate)
        }
        else
        {
            throw UnsupportedExpressionException("Unsupported term of type ${predicate.javaClass}")
        }
    }

    override fun defineRule(rule: Rule) {
        this.rules.add(rule)
    }
}