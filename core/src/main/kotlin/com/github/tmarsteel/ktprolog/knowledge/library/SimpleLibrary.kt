package com.github.tmarsteel.ktprolog.knowledge.library

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
}