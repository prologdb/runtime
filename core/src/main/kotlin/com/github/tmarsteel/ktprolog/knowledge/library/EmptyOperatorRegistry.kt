package com.github.tmarsteel.ktprolog.knowledge.library

object EmptyOperatorRegistry : OperatorRegistry {
    override fun getPrefixDefinition(name: String): OperatorDefinition? = null

    override fun getInfixDefinition(name: String): OperatorDefinition? = null

    override fun getPostfixDefinition(name: String): OperatorDefinition? = null

    override val allOperators: Iterable<OperatorDefinition> = emptySet()
}