package com.github.prologdb.runtime.module

import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.proofsearch.Authorization
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.util.OperatorRegistry

/**
 * A module, as results from reading/consulting a prolog file.
 */
interface Module {
    val name: String

    val exportedPredicates: Map<ClauseIndicator, PrologCallable>

    val imports: List<ModuleImport>

    /**
     * Operators available in this module (including those given as context operators when parsing)
     */
    val localOperators: OperatorRegistry

    fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext {
        return createProofSearchContext(
            deriveFrom.principal,
            deriveFrom.randomVariableScope,
            deriveFrom.authorization,
            deriveFrom.rootAvailableModules
        )
    }

    fun createProofSearchContext(principal: Principal, randomVariableScope: RandomVariableScope,
                                 authorization: Authorization, rootAvailableModules: Map<String, Module>): ProofSearchContext
}

