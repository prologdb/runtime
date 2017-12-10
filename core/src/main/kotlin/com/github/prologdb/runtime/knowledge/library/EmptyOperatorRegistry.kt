package com.github.prologdb.runtime.knowledge.library

object EmptyOperatorRegistry : OperatorRegistry {
    override val allOperators: Set<OperatorDefinition> = emptySet()
    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> = allOperators
}