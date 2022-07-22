package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.util.OperatorRegistry

/**
 * A module, as results from reading/consulting a prolog file. An instance
 * of this class is always bound to a [PrologRuntimeEnvironment] because
 * of imported operators that affect semantics of the parsed code.
 */
interface Module {
    val declaration: ModuleDeclaration

    val exportedPredicates: Map<ClauseIndicator, PrologCallable>
    val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>

    val imports: List<ModuleImport>

    val localOperators: OperatorRegistry
}

