package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.util.OperatorRegistry

/**
 * A module, as results from reading/consulting a prolog file.
 */
interface Module {
    val name: String

    val exportedPredicates: Map<ClauseIndicator, PrologCallable>
    val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>

    val imports: List<ModuleImport>

    /**
     * Operators available in this module (including those given as context operators when parsing)
     */
    val localOperators: OperatorRegistry
}

