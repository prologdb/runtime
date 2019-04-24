package com.github.prologdb.parser.parser

import com.github.prologdb.runtime.util.MutableOperatorRegistry
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorRegistry

/**
 * A [MutableOperatorRegistry] that tracks the changes made to an
 * existing [MutableOperatorRegistry].
 */
class DiffOperatorRegistry(val nested: OperatorRegistry) : MutableOperatorRegistry {

    /** Additional operator definitions. */
    private val additional = mutableMapOf<String, MutableSet<OperatorDefinition>>()

    /** Definitions from [nested] that have been overridden by additions in [additional]. */
    private val overridden = mutableMapOf<String, MutableSet<OperatorDefinition>>()

    /**
     * All newly added definitions.
     */
    val additionalDefinitions: Iterable<OperatorDefinition>
        get() = additional.values.flatten()

    /**
     * The definitions that were overridden as a result of
     * defining those in [additionalDefinitions]
     */
    val overriddenDefinitions: Iterable<OperatorDefinition>
        get() = overridden.values.flatten()

    override fun getOperatorDefinitionsFor(name: String): Set<OperatorDefinition> {
        val fromSelf = additional[name] as Set<OperatorDefinition>? ?: emptySet()
        val overridden = this.overridden[name] as Set<OperatorDefinition>? ?: emptySet()

        val fromNested = nested.getOperatorDefinitionsFor(name) - overridden
        return fromSelf + fromNested
    }

    private var _cachedAll: Set<OperatorDefinition>? = null

    override val allOperators: Iterable<OperatorDefinition>
        get() {
            return _cachedAll ?: {
                val cached = nested.allOperators.toMutableSet()
                overridden.values.forEach { it.forEach { cached.remove(it) } }
                additional.values.forEach { cached.addAll(it) }
                _cachedAll = cached
                cached
            }()
        }

    override fun defineOperator(definition: OperatorDefinition) {
        // invalidate cache for [allOperators]
        _cachedAll = null

        // check for overrides
        val overridden = nested.getOperatorDefinitionsFor(definition.name)
            .filter { it.type.isSameArgumentRelationAs(definition.type) }

        if (overridden.isNotEmpty()) {
            this.overridden.computeIfAbsent(definition.name, { _ -> HashSet() }).addAll(overridden)
        }

        additional.computeIfAbsent(definition.name, { _ -> HashSet() }).add(definition)
    }

    /**
     * An [OperatorRegistry] that contains only the delta between [nested] and
     * `this`. This will always be the same object; changes to `this` directly
     * reflect onto this value.
     */
    val deltaRegistry: OperatorRegistry = object : OperatorRegistry {
        override fun getOperatorDefinitionsFor(name: String) = additional[name] as Set<OperatorDefinition>? ?: emptySet()

        override val allOperators: Iterable<OperatorDefinition>
            get() = additionalDefinitions
    }
}