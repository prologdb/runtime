package com.github.prologdb.parser

import com.github.prologdb.runtime.ClauseIndicator

class ModuleDeclaration(
    val moduleName: String,
    /**
     * The predicates to export. If null, all predicates are exported.
     */
    val exportedPredicates: Set<ClauseIndicator>? = null
)