package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.PrologRuntimeException
import com.github.tmarsteel.ktprolog.RandomVariableScope
import com.github.tmarsteel.ktprolog.VariableMapping
import com.github.tmarsteel.ktprolog.knowledge.builtin.IdentityPredicate
import com.github.tmarsteel.ktprolog.knowledge.builtin.IsAtomPredicate
import com.github.tmarsteel.ktprolog.knowledge.builtin.NegationRule
import com.github.tmarsteel.ktprolog.knowledge.library.DoublyIndexedLibrary
import com.github.tmarsteel.ktprolog.knowledge.library.Library
import com.github.tmarsteel.ktprolog.query.PredicateQuery
import com.github.tmarsteel.ktprolog.term.Predicate
import com.github.tmarsteel.ktprolog.term.Variable
import com.github.tmarsteel.ktprolog.unification.Unification
import kotlin.coroutines.experimental.buildSequence

class DefaultKnowledgeBase : MutableKnowledgeBase {
    private val library = DoublyIndexedLibrary()

    override fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope): Sequence<Unification> {
        // replace all variables in the term with random ones to prevent name collisions
        val termMappings = VariableMapping()
        val replaced = randomVarsScope.withRandomVariables(predicate, termMappings)

        return buildSequence<Unification> {
            for (libEntry in library.findFor(predicate)) {
                if (libEntry is Predicate) {
                    val knownPredicateReplaced = randomVarsScope.withRandomVariables(libEntry, VariableMapping())
                    val unification = knownPredicateReplaced.unify(replaced)
                    if (unification != null) {
                        val resolvedBucket = unification.variableValues.withVariablesResolvedFrom(termMappings)
                        resolvedBucket.retainAll(predicate.variables)
                        yield(Unification(resolvedBucket))
                    }
                }
                else if (libEntry is Rule) {
                    yieldAll(libEntry.fulfill(predicate, this@DefaultKnowledgeBase, randomVarsScope))
                }
                else {
                    throw PrologRuntimeException("Unsupported library entry $libEntry")
                }
            }
        }
    }

    override fun assert(predicate: Predicate) {
        library.add(predicate)
    }

    override fun defineRule(rule: Rule) {
        library.add(rule)
    }

    override fun load(library: Library) {
        this.library.include(library)
    }

    init {
        val A = Variable("A")
        val B = Variable("B")
        val X = Variable("X")

        // EQUALITY
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

        // TYPE SAFETY
        assert(IsAtomPredicate)
    }
}