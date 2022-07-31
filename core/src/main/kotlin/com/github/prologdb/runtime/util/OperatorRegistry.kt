package com.github.prologdb.runtime.util

/**
 * Holds operators defined in a knowledge base. This might seem like a concern of the parser since, at the core level,
 * it does not make a difference whether `+(1,2)` was parsed from `1 + 2` or from `+(1,2)`. But operator definitions
 * are at the very heart of the language (e.g. =/2). It would be possible to define the core without the syntax sugar
 * but that would make it a real chore to go from core + parser to a working, compliant REPL. The core concerns itself
 * with operator definitions because it makes the library easier to use.
 */
interface OperatorRegistry {
    /**
     * Returns all definitions for operators with the given name
     */
    fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition>

    /**
     * Is supposed to be used to display listings and merge multiple operator registries.
     * Should be computed on demand only.
     */
    val allOperators: Iterable<OperatorDefinition>
}

interface OperatorRegistrationTarget {
    /**
     * (Re-)defines the given operator (overriding existing definitions for
     * the same [OperatorDefinition.name] if present).
     */
    fun defineOperator(definition: OperatorDefinition)

    fun include(other: OperatorRegistry) {
        other.allOperators.forEach(this::defineOperator)
    }
}

interface MutableOperatorRegistry : OperatorRegistry, OperatorRegistrationTarget

/**
 * Defines an operator to use in a prolog program.
 */
data class OperatorDefinition (
    /**
     * The precedence, between 0 and 1200.
     */
    val precedence: Short,

    /**
     * The type of this operator ; defines how it relates towards its surroundings
     */
    val type: OperatorType,

    /**
     * The functor of the operator
     */
    val name: String
) {
    init {
        assert(precedence >= 0)
        assert(precedence <= 1200)
        assert(name.isNotEmpty())
        assert(name.none { it.isWhitespace() })
    }

    override fun toString() = "op($precedence, ${type.name.lowercase()}, $name)"
}

enum class OperatorType {
    FX,
    FY,
    XFX,
    XFY,
    YFX,
    XF,
    YF;

    val isPrefix by lazy { this == FX || this == FY }
    val isInfix by lazy { this == XFX || this == XFY || this == YFX }
    val isPostfix by lazy { this == XF || this == YF }
    
    val arity: Int by lazy { if (isPrefix || isPostfix) 1 else 2 }

    /**
     * Whether the argument positioning around the operator type
     * is the same as with the given.
     */
    fun isSameArgumentRelationAs(other: OperatorType): Boolean {
        return when {
            other === this -> true
            this.arity != other.arity -> false
            this.isPrefix && other.isPrefix -> true
            this.isPostfix && other.isPostfix -> true
            this.isInfix && other.isInfix -> true
            else -> false
        }
    }
}

/**
 * A simple implementation of [MutableOperatorRegistry].
 */
class DefaultOperatorRegistry : MutableOperatorRegistry {

    private val operators: OperatorMap = mutableMapOf()

    constructor(definitions: Set<OperatorDefinition> = emptySet()) {
        definitions.forEach(this::defineOperator)
    }

    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> = operators[name] ?: emptySet()

    override fun defineOperator(definition: OperatorDefinition) {
        val targetSet = operators.computeIfAbsent(definition.name, { HashSet() })

        // remove overridden definitions
        targetSet.removeIf { it.type.isSameArgumentRelationAs(definition.type) }

        targetSet.add(definition)
    }

    override val allOperators: Iterable<OperatorDefinition>
        get() = operators.values.flatten()
}

private typealias OperatorMap = MutableMap<String, MutableSet<OperatorDefinition>>

object EmptyOperatorRegistry : OperatorRegistry {
    override val allOperators: Set<OperatorDefinition> = emptySet()
    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> = allOperators
}