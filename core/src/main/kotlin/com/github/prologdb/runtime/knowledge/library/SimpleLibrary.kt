package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.lazysequence.LazySequence
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.unification.Unification

open class SimpleLibrary(
    private val entryStore: MutableLibraryEntryStore,
    private val operatorRegistry: MutableOperatorRegistry
) : MutableLibrary {
    override val exports: Iterable<LibraryEntry>
        get() = entryStore.exports

    override fun add(entry: LibraryEntry) = entryStore.add(entry)

    override val allOperators: Iterable<OperatorDefinition>
        get() = operatorRegistry.allOperators

    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> = operatorRegistry.getOperatorDefinitionsFor(name)

    override fun defineOperator(definition: OperatorDefinition) = operatorRegistry.defineOperator(definition)

    override fun retractFact(fact: Predicate): LazySequence<Unification> {
        return entryStore.retractFact(fact)
    }

    override fun retract(unifiesWith: Predicate): LazySequence<Unification> {
        return entryStore.retract(unifiesWith)
    }
}