package com.github.prologdb.runtime.module

import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
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

data class ModuleReference(
    /**
     * The alias for the path pointing to the module files parent directory,
     * e.g. `library` or `system`.
     */
    val pathAlias: String,

    val moduleName: String
) {
    override fun toString(): String {
        return "$pathAlias($moduleName)"
    }
}

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

/**
 * A [Module], constructed from AST. Predicates will be interpreted when
 * invoked; `:- compile/1` can exchange certain AST-based [PrologCallable]s
 * for compiled versions of themselves.
 */
class ASTModule(
    override val name: String,
    override val imports: List<ModuleImport>,
    val givenClauses: Iterable<Clause>,
    val dynamicPredicates: Set<ClauseIndicator>,
    val exportedPredicateIndicators: Set<ClauseIndicator>,
    override val localOperators: OperatorRegistry = ISOOpsOperatorRegistry
) : Module {
    private val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>
    init {
        val _allDeclaredPredicates = givenClauses.asSequence()
            .groupingBy { ClauseIndicator.of(it) }
            .foldTo(
                HashMap(),
                { indicator, _ -> ASTPrologPredicate(indicator, this) },
                { _, astPredicate, clause ->
                    astPredicate.assertz(clause)
                    astPredicate
                }
            )

        for (dynamicClauseIndicator in dynamicPredicates) {
            _allDeclaredPredicates.computeIfAbsent(dynamicClauseIndicator) { indicator -> ASTPrologPredicate(indicator, this) }
        }

        allDeclaredPredicates = _allDeclaredPredicates

        allDeclaredPredicates.values.forEach {
            if (it.indicator !in dynamicPredicates) {
                it.seal()
            }
        }
    }

    override val exportedPredicates: Map<ClauseIndicator, PrologCallable> = allDeclaredPredicates
        .filter { it.key in exportedPredicateIndicators }

    override fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext {
        if (deriveFrom is ModuleScopeProofSearchContext && deriveFrom.module == this) {
            return deriveFrom
        }

        return super.deriveScopedProofSearchContext(deriveFrom)
    }

    override fun createProofSearchContext(principal: Principal, randomVariableScope: RandomVariableScope,
                                          authorization: Authorization, rootAvailableModules: Map<String, Module>): ProofSearchContext {
        return ModuleScopeProofSearchContext(
            this,
            this.allDeclaredPredicates,
            principal,
            randomVariableScope,
            authorization,
            rootAvailableModules
        )
    }
}
