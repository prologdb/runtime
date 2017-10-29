package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.VariableMapping
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.unification.Unification
import kotlin.coroutines.experimental.buildSequence

class DefaultKnowledgeBase : MutableKnowledgeBase {
    private val predicates: MutableSet<Predicate> = HashSet()
    private val rules: MutableSet<Rule> = mutableSetOf()

    override fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope): Sequence<Unification> {
        // replace all variables in the term with random ones to prevent name collisions
        val termMappings = VariableMapping()
        val replaced = randomVarsScope.withRandomVariables(predicate, termMappings)

        return buildSequence<Unification> {
            for (knownPredicate in predicates) {
                val knownPredicateReplaced = randomVarsScope.withRandomVariables(knownPredicate, VariableMapping())
                val unification = knownPredicateReplaced.unify(replaced)
                if (unification != null) {
                    val resolvedBucket = unification.variableValues.withVariablesResolvedFrom(termMappings)
                    resolvedBucket.retainAll(predicate.variables)
                    yield(Unification(resolvedBucket))
                }
            }

            for (knownRule in rules) {
                yieldAll(knownRule.fulfill(predicate, this@DefaultKnowledgeBase, randomVarsScope))
            }
        }
    }

    override fun assert(predicate: Predicate) {
        predicates.add(predicate)
    }

    override fun defineRule(rule: Rule) {
        this.rules.add(rule)
    }
}