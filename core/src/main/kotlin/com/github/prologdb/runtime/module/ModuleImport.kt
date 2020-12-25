package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.ClauseIndicator

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
        val imports: Map<ClauseIndicator, String>
    ) : ModuleImport()

    data class Except(
        override val moduleReference: ModuleReference,
        val excluded: Set<ClauseIndicator>
    ) : ModuleImport()
}
