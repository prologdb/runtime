package com.github.tmarsteel.ktprolog.knowledge.library

import com.github.tmarsteel.ktprolog.knowledge.library.OperatorType.*

private typealias OperatorMap = MutableMap<String,OperatorDefinition>

/**
 * A simple implementation of [MutableOperatorRegistry]. Includes the definitions of the ISO prolog operators in
 * every instance
 * @param withIsoOps If true, the absolutely essential operators will be defined at construction time. These are:
 *
 * * TODO
 *
 * This parameter should be set to false unless you are creating a prolog runtime environment
 * from scratch. E.g., [DefaultKnowledgeBase] sets it to true.
 */
class DefaultOperatorRegistry(withIsoOps: Boolean) : MutableOperatorRegistry {

    private val prefixOps: OperatorMap = mutableMapOf()
    private val infixOps: OperatorMap = mutableMapOf()
    private val postfixOps: OperatorMap = mutableMapOf()

    /** Delegates to `this(false)`. */
    constructor() : this(false)

    init {
        if (withIsoOps) {
            defineOperator(OperatorDefinition(1200, XFX, ":-"))

            defineOperator(OperatorDefinition(1200, FX, ":-"))
            defineOperator(OperatorDefinition(1200, FX, "?-"))

            defineOperator(OperatorDefinition(1150, FX, "dynamic"))

            defineOperator(OperatorDefinition(1100, XFY, ";"))
            defineOperator(OperatorDefinition(1100, XFY, "|"))

            defineOperator(OperatorDefinition(1000, XFY, ","))

            // equality and inequality operators are defined in EqualityLibrary
            // mathematic operators are defined in MathLibrary; /2 is an exception
            // because it is used to denote predicates with dynamic, e.g.: :- dynamic predicateName/3.
            defineOperator(OperatorDefinition(400, YFX, "/"))
        }
    }

    override fun getPrefixDefinition(name: String): OperatorDefinition? = prefixOps[name]

    override fun getInfixDefinition(name: String): OperatorDefinition? = infixOps[name]

    override fun getPostfixDefinition(name: String): OperatorDefinition? = postfixOps[name]

    override fun defineOperator(definition: OperatorDefinition) {
        val targetMap = when(definition.type) {
            FX, FY -> prefixOps
            XFX, XFY, YFX -> infixOps
            XF, YF -> postfixOps
        }

        targetMap[definition.name] = definition
    }

    override val allOperators: Iterable<OperatorDefinition>
        get() = prefixOps.values + infixOps.values + postfixOps.values

    override fun include(other: OperatorRegistry) {
        if (other is DefaultOperatorRegistry) {
            prefixOps.putAll(other.prefixOps)
            infixOps.putAll(other.infixOps)
            postfixOps.putAll(other.postfixOps)
        } else {
            super.include(other)
        }
    }
}