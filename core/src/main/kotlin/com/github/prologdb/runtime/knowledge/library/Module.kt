package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.knowledge.ASTPrologPredicate
import com.github.prologdb.runtime.knowledge.PrologCallable

/**
 * A module, as results from reading/consulting a prolog file.
 */
interface Module {
    val name: String

    val exportedPredicates: Map<ClauseIndicator, PrologCallable>

    /**
     * The operators defined and exported by this module (using `:- op/3`).
     */
    val exportedOperators: OperatorRegistry

    /**
     * Resolves the callable for [indicator].
     *
     * @return the resolved import or null if no matching import is declared.
     */
    fun findImport(indicator: ClauseIndicator): PrologCallable?
}

sealed class ModuleImport(
    val module: Module
)
class FullModuleImport(module: Module) : ModuleImport(module)
class PartialModuleImport(
    module: Module,
    /**
     * The imported predicates. The key may contain an alias, e.g.:
     * `use_module(foo, [bar/1 as baz])` will result in a key `baz/1` referring
     * to the `bar/1` predicate of module `foo`.
     */
    val imports: Map<ClauseIndicator, PrologCallable>
) : ModuleImport(module)

/**
 * A [Module], constructed from AST. Predicates will be interpreted when
 * invoked; `:- compile/1` can exchange certain AST-based [PrologCallable]s
 * for compiled versions of themselves.
 */
class ASTModule(
    override val name: String,
    val imports: List<ModuleImport>,
    val givenClauses: Iterable<Clause>,
    val dynamicExports: Set<ClauseIndicator>,
    override val exportedOperators: OperatorRegistry
) : Module {
    override val exportedPredicates: Map<ClauseIndicator, PrologCallable>
    init {
        exportedPredicates = givenClauses.asSequence()
            .groupingBy { ClauseIndicator.of(it) }
            .fold(
                { indicator, firstClause ->
                    val astPredicate = ASTPrologPredicate(indicator)
                    astPredicate.assertz(firstClause)
                    astPredicate
                },
                { _, astPredicate, clause ->
                    astPredicate.assertz(clause)
                    astPredicate
                }
            )

        exportedPredicates.values.forEach {
            if (it.indicator !in dynamicExports) {
                it.seal()
            }
        }
    }

    override fun findImport(indicator: ClauseIndicator): PrologCallable? {
        // TODO: optimize to a Map<ClauseIndicator, PrologCallable> for O(1) lookup
        return imports
            .asSequence()
            .map { import ->
                when (import) {
                    is FullModuleImport -> import.module.exportedPredicates[indicator]
                    is PartialModuleImport -> import.imports[indicator]
                }
            }
            .firstOrNull()
    }
}