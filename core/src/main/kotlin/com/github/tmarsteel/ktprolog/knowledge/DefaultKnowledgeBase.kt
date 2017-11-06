package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.VariableMapping
import com.github.tmarsteel.ktprolog.query.PredicateQuery
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Variable
import com.github.tmarsteel.ktprolog.unification.Unification
import kotlin.coroutines.experimental.buildSequence

class DefaultKnowledgeBase : MutableKnowledgeBase {
    private val elements: MutableList<Any> = mutableListOf()

    override fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope): Sequence<Unification> {
        // replace all variables in the term with random ones to prevent name collisions
        val termMappings = VariableMapping()
        val replaced = randomVarsScope.withRandomVariables(predicate, termMappings)

        return buildSequence<Unification> {
            for (element in elements) {
                if (element is Predicate) {
                    val knownPredicateReplaced = randomVarsScope.withRandomVariables(element, VariableMapping())
                    val unification = knownPredicateReplaced.unify(replaced)
                    if (unification != null) {
                        val resolvedBucket = unification.variableValues.withVariablesResolvedFrom(termMappings)
                        resolvedBucket.retainAll(predicate.variables)
                        yield(Unification(resolvedBucket))
                    }
                }
                else if (element is Rule) {
                    yieldAll(element.fulfill(predicate, this@DefaultKnowledgeBase, randomVarsScope))
                }
            }
        }
    }

    override fun assert(predicate: Predicate) {
        elements.add(predicate)
    }

    override fun defineRule(rule: Rule) {
        elements.add(rule)
    }

    init {
        val A = Variable("A")
        val B = Variable("B")
        val X = Variable("X")

        // unification predicate
        assert(Predicate("=", arrayOf(X, X)))

        defineRule(NegationRule)

        // \=(A, B) :- not(=(A, B)).
        defineRule(Rule(
            Predicate("\\=", arrayOf(A, B)),
            PredicateQuery(
                Predicate("not", arrayOf(
                    Predicate("=", arrayOf(A, B))
                ))
            )
        ))

        assert(IdentityPredicate)

        // \==(A, B) :- not(==(A, B)).
        defineRule(Rule(
            Predicate("\\==", arrayOf(A, B)),
            PredicateQuery(
                Predicate("not", arrayOf(
                    Predicate("==", arrayOf(A, B))
                ))
            )
        ))
    }
}