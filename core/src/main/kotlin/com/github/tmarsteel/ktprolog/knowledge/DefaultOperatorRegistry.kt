package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.knowledge.library.MutableOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorDefinition
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorRegistry
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

    constructor() : this(true)

    init {
        if (withIsoOps) {
            defineOperator(OperatorDefinition(1200, XFX, ":-"))

            defineOperator(OperatorDefinition(1200, FX, ":-"))
            defineOperator(OperatorDefinition(1200, FX, "?-"))

            defineOperator(OperatorDefinition(1150, FX, "dynamic"))

            defineOperator(OperatorDefinition(1100, XFY, ";"))

            defineOperator(OperatorDefinition(1000, XFY, ","))

            // equality and inequality operators are defined in EqualityLibrary
            defineOperator(OperatorDefinition(700, XFX, "<"))
            defineOperator(OperatorDefinition(700, XFX, "=<"))
            defineOperator(OperatorDefinition(700, XFX, "=\\="))
            defineOperator(OperatorDefinition(700, XFX, ">"))
            defineOperator(OperatorDefinition(700, XFX, ">="))
            defineOperator(OperatorDefinition(700, XFX, "is"))

            defineOperator(OperatorDefinition(500, YFX, "+"))
            defineOperator(OperatorDefinition(500, YFX, "-"))
            defineOperator(OperatorDefinition(500, YFX, "xor"))

            defineOperator(OperatorDefinition(400, YFX, "*"))
            defineOperator(OperatorDefinition(400, YFX, "/"))
            defineOperator(OperatorDefinition(400, YFX, "mod"))

            defineOperator(OperatorDefinition(200, XFX, "**"))

            defineOperator(OperatorDefinition(200, XFY, "^"))

            defineOperator(OperatorDefinition(200, FY, "+"))
            defineOperator(OperatorDefinition(200, FY, "-"))
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