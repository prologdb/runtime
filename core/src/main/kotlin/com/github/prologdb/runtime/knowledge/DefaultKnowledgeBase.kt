package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.VariableMapping
import com.github.prologdb.runtime.builtin.ComparisonLibrary
import com.github.prologdb.runtime.builtin.EqualityLibrary
import com.github.prologdb.runtime.builtin.dict.DictLibrary
import com.github.prologdb.runtime.builtin.dynamic.DynamicsLibrary
import com.github.prologdb.runtime.builtin.lists.ListsLibrary
import com.github.prologdb.runtime.builtin.math.MathLibrary
import com.github.prologdb.runtime.builtin.string.StringsLibrary
import com.github.prologdb.runtime.builtin.typesafety.TypeSafetyLibrary
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

class DefaultKnowledgeBase(val library: MutableLibrary) : MutableKnowledgeBase {

    constructor() : this(SimpleLibrary(
        DoublyIndexedLibraryEntryStore(),
        DefaultOperatorRegistry(true)
    )) {
        load(EqualityLibrary)
        load(ComparisonLibrary)
        load(TypeSafetyLibrary)
        load(MathLibrary)
        load(StringsLibrary)
        load(ListsLibrary)
        load(DictLibrary)
        load(DynamicsLibrary)
    }

    override val operatorRegistry = library

    override fun fulfill(predicate: Predicate, context: ProofSearchContext): LazySequence<Unification> {
        if (context.knowledgeBase != this) throw IllegalArgumentException("Given context must belong to the same knowledge base")

        // replace all variables in the term with random ones to prevent name collisions
        val termMappings = VariableMapping()
        val replaced = context.randomVariableScope.withRandomVariables(predicate, termMappings)

        return buildLazySequence<Unification>(context.principal) {
            for (libEntry in library.findFor(predicate)) {
                if (libEntry is Predicate) {
                    val knownPredicateReplaced = context.randomVariableScope.withRandomVariables(libEntry, VariableMapping())
                    val unification = knownPredicateReplaced.unify(replaced)
                    if (unification != null) {
                        val resolvedBucket = unification.variableValues.withVariablesResolvedFrom(termMappings)
                        resolvedBucket.retainAll(predicate.variables)
                        yield(Unification(resolvedBucket))
                    }
                }
                else {
                    libEntry.unifyWithKnowledge(this, predicate, context)
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