package com.github.prologdb.runtime.knowledge

import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.builtin.EqualityLibrary
import com.github.prologdb.runtime.builtin.MathLibrary
import com.github.prologdb.runtime.builtin.StringsLibrary
import com.github.prologdb.runtime.builtin.TypeSafetyLibrary
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.lazysequence.buildLazySequence
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

class DefaultKnowledgeBase(val library: MutableLibrary) : MutableKnowledgeBase {

    constructor() : this(SimpleLibrary(
        DoublyIndexedLibraryEntryStore(),
        DefaultOperatorRegistry(true)
    )) {
        load(EqualityLibrary)
        load(TypeSafetyLibrary)
        load(MathLibrary)
        load(StringsLibrary)
    }

    override val operatorRegistry = library

    override fun fulfill(predicate: Predicate, randomVarsScope: RandomVariableScope): LazySequence<Unification> {
        // replace all variables in the term with random ones to prevent name collisions
        val termMappings = VariableMapping()
        val replaced = randomVarsScope.withRandomVariables(predicate, termMappings)

        return buildLazySequence<Unification> {
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
                else {
                    yieldAll(libEntry.unifyWithKnowledge(predicate, this@DefaultKnowledgeBase, randomVarsScope))
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
}