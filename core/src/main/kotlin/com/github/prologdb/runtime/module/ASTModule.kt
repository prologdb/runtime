package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.DeterministicDynamicPrologPredicate
import com.github.prologdb.runtime.proofsearch.DynamicPrologPredicate
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.util.EmptyOperatorRegistry
import com.github.prologdb.runtime.util.OperatorRegistry

/**
 * A [Module], constructed from AST. Predicates will be interpreted when
 * invoked; `:- compile/1` can exchange certain AST-based [PrologCallable]s
 * for compiled versions of themselves.
 */
class ASTModule(
    override val declaration: ModuleDeclaration,
    override val imports: List<ModuleImport>,
    val givenClauses: Iterable<Clause>,
    val dynamicPredicates: Set<ClauseIndicator>,
    val moduleTransparents: Set<ClauseIndicator>,
    val deterministics: Set<ClauseIndicator>,
    val exportedPredicateIndicators: Set<ClauseIndicator>,
    override val localOperators: OperatorRegistry = EmptyOperatorRegistry,
) : Module {
    override val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>

    init {
        val _allDeclaredPredicates = givenClauses.asSequence()
            .groupingBy { ClauseIndicator.of(it) }
            .foldTo(
                HashMap(),
                { indicator, _ ->
                    var predicate: DynamicPrologPredicate = ASTPrologPredicate(
                        indicator,
                        this,
                        indicator in moduleTransparents
                    )
                    if (indicator in deterministics) {
                        predicate = DeterministicDynamicPrologPredicate(predicate)
                    }

                    predicate
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
            if (ClauseIndicator.of(it) !in dynamicPredicates) {
                it.seal()
            }
        }
    }

    override val exportedPredicates: Map<ClauseIndicator, PrologCallable> = allDeclaredPredicates
        .filter { it.key in exportedPredicateIndicators }
}
