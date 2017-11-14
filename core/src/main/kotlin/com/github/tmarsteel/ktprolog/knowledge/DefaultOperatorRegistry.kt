package com.github.tmarsteel.ktprolog.knowledge

import com.github.tmarsteel.ktprolog.knowledge.library.OperatorType.*
import com.github.tmarsteel.ktprolog.knowledge.library.MutableOperatorRegistry
import com.github.tmarsteel.ktprolog.knowledge.library.OperatorDefinition
import kotlin.coroutines.experimental.buildSequence

private typealias OperatorMap = MutableMap<String,MutableSet<OperatorDefinition>>

/**
 * A simple implementation of [MutableOperatorRegistry]. Includes the definitions of the ISO prolog operators in
 * every instance
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

            defineOperator(OperatorDefinition(700, XFX, "<"))
            defineOperator(OperatorDefinition(700, XFX, "="))
            defineOperator(OperatorDefinition(700, XFX, "=<"))
            defineOperator(OperatorDefinition(700, XFX, "=="))
            defineOperator(OperatorDefinition(700, XFX, "=\\="))
            defineOperator(OperatorDefinition(700, XFX, ">"))
            defineOperator(OperatorDefinition(700, XFX, ">="))
            defineOperator(OperatorDefinition(700, XFX, "\\="))
            defineOperator(OperatorDefinition(700, XFX, "\\=="))
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

    override fun findPrefixOperators(name: String): Set<OperatorDefinition> = prefixOps[name] ?: emptySet()

    override fun findInfixOperators(name: String): Set<OperatorDefinition> = infixOps[name] ?: emptySet()

    override fun findPostfixOperators(name: String): Set<OperatorDefinition> = postfixOps[name] ?: emptySet()

    override fun defineOperator(definition: OperatorDefinition) {
        val targetMap = when(definition.type) {
            FX, FY -> prefixOps
            XFX, XFY, YFX -> infixOps
            XF, YF -> postfixOps
        }

        val targetSet: MutableSet<OperatorDefinition>
        if (definition.name !in targetMap) {
            targetSet = mutableSetOf()
            targetMap[definition.name] = targetSet
        } else {
            targetSet = targetMap[definition.name]!!
        }

        targetSet.add(definition)
    }

    override val allOperators: Iterable<OperatorDefinition>
        get() = (prefixOps.values + infixOps.values + postfixOps.values).flatMap { it }
}