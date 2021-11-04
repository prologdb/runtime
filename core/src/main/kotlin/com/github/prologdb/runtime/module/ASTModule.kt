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
 * A [Module], constructed from AST. Predicates will be interpreted when
 * invoked; `:- compile/1` can exchange certain AST-based [PrologCallable]s
 * for compiled versions of themselves.
 */
class ASTModule(
    override val name: String,
    override val imports: List<ModuleImport>,
    val givenClauses: Iterable<Clause>,
    val dynamicPredicates: Set<ClauseIndicator>,
    val moduleTransparents: Set<ClauseIndicator>,
    val exportedPredicateIndicators: Set<ClauseIndicator>,
    override val localOperators: OperatorRegistry = ISOOpsOperatorRegistry
) : Module {
    private val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>

    init {
        val _allDeclaredPredicates = givenClauses.asSequence()
            .groupingBy { ClauseIndicator.of(it) }
            .foldTo(
                HashMap(),
                { indicator, _ ->
                    ASTPrologPredicate(
                        indicator,
                        this,
                        indicator in moduleTransparents
                    )
                },
                { _, astPredicate, clause ->
                    astPredicate.assertz(clause)
                    astPredicate
                }
            )

        for (dynamicClauseIndicator in dynamicPredicates) {
            _allDeclaredPredicates.computeIfAbsent(dynamicClauseIndicator) { indicator ->
                ASTPrologPredicate(indicator, this, indicator in moduleTransparents)
            }
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
