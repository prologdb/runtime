package com.github.tmarsteel.ktprolog.knowledge.library

object EmptyOperatorRegistry : OperatorRegistry {
    override val allOperators: Set<OperatorDefinition> = emptySet()
    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> = allOperators
}