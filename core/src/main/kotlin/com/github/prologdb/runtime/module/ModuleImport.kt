package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType

sealed class ModuleImport {
    abstract val moduleReference: ModuleReference

    data class Full(override val moduleReference: ModuleReference) : ModuleImport()

    data class Selective(
        override val moduleReference: ModuleReference,

        /**
         * The imported predicates. The key refers to the indicator of the predicate as exported
         * by the imported module. The value is the functor to be used in the code that imports
         * the module. E.g. `:- use_module(foo, [bar/1 as baz])` will result in the key being
         * `bar/1` and the value being `baz`.
         */
        val predicates: Map<ClauseIndicator, String>,
        val operators: Set<OperatorImport>,
    ) : ModuleImport()

    data class Except(
        override val moduleReference: ModuleReference,
        val excludedPredicates: Set<ClauseIndicator>,
        val excludedOperators: Set<OperatorImport>,
    ) : ModuleImport()

    data class OperatorImport(
        /** If non-null: the precedence to import; if null: unbound, import operators with any precedence */
        val precedence: Short?,

        /** If non-null: the type to import; if null: unbound, import operators of all types */
        val type: OperatorType?,

        /** If non-null: the name to import; if null: unbound, import operators with any name */
        val name: String?,
    ) {
        fun matches(definition: OperatorDefinition): Boolean {
            return precedence == null || precedence == definition.precedence
                && type == null || type == definition.type
                && name == null || name == definition.name
        }
    }
}
