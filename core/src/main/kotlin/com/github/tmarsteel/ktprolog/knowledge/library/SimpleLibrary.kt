package com.github.tmarsteel.ktprolog.knowledge.library

class SimpleLibrary(
    private val entryStore: MutableLibraryEntryStore,
    private val operatorRegistry: MutableOperatorRegistry
) : MutableLibrary {
    override val exports: Iterable<LibraryEntry>
        get() = entryStore.exports

    override fun add(entry: LibraryEntry) = entryStore.add(entry)

    override fun getPrefixDefinition(name: String): OperatorDefinition? = operatorRegistry.getPrefixDefinition(name)

    override fun getInfixDefinition(name: String): OperatorDefinition? = operatorRegistry.getInfixDefinition(name)

    override fun getPostfixDefinition(name: String): OperatorDefinition? = operatorRegistry.getPostfixDefinition(name)

    override val allOperators: Iterable<OperatorDefinition>
        get() = operatorRegistry.allOperators

    override fun defineOperator(definition: OperatorDefinition) = operatorRegistry.defineOperator(definition)
}