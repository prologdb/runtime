package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.util.EmptyOperatorRegistry
import com.github.prologdb.runtime.util.OperatorRegistry

class ModuleDeclaration(
    val moduleName: String,
    /**
     * The predicates to export. If null, all predicates are exported.
     */
    val exportedPredicates: Set<ClauseIndicator>? = null,
    val exportedOperators: OperatorRegistry = EmptyOperatorRegistry,
)