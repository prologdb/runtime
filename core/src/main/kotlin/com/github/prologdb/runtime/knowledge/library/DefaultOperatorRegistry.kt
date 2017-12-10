package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.knowledge.library.OperatorType.*

private typealias OperatorMap = MutableMap<String,MutableSet<OperatorDefinition>>

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

    private val operators: OperatorMap = mutableMapOf()

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

    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> = operators[name] ?: emptySet()

    override fun defineOperator(definition: OperatorDefinition) {
        var targetSet = operators[definition.name]
        if (targetSet == null) {
            targetSet = HashSet()
            operators[definition.name] = targetSet
        }

        targetSet.add(definition)
    }

    override val allOperators: Iterable<OperatorDefinition>
        get() = operators.values.flatten()

    override fun include(other: OperatorRegistry) {
        if (other is DefaultOperatorRegistry) {
            for (name in other.operators.keys) {
                if (name in this.operators) {
                    this.operators[name]!!.addAll(other.operators[name]!!)
                } else {
                    this.operators[name] = mutableSetOf()
                }
            }
        } else {
            super.include(other)
        }
    }
}