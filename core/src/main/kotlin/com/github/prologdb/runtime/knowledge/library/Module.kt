package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.knowledge.ASTPrologPredicate
import com.github.prologdb.runtime.knowledge.ModuleScopeProofSearchContext
import com.github.prologdb.runtime.knowledge.PrologCallable
import com.github.prologdb.runtime.knowledge.ProofSearchContext

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
     * Among all imports into this moudle, resolves the [PrologCallable] that,
     * within the scope of the module (aliasing), is referenced by the given
     * [ClauseIndicator].
     *
     * @return the resolved import or null if no matching import is declared.
     */
    fun findImport(indicator: ClauseIndicator): PrologCallable?

    fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext
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
    val dynamicPredicates: Set<ClauseIndicator>,
    val exportedPredicateIndicators: Set<ClauseIndicator>,
    override val exportedOperators: OperatorRegistry
) : Module {
    private val allDeclaredPredicates: Map<ClauseIndicator, PrologCallable>
    init {
        allDeclaredPredicates = givenClauses.asSequence()
            .groupingBy { ClauseIndicator.of(it) }
            .fold(
                { indicator, _ -> ASTPrologPredicate(indicator, this) },
                { _, astPredicate, clause ->
                    astPredicate.assertz(clause)
                    astPredicate
                }
            )

        allDeclaredPredicates.values.forEach {
            if (it.indicator !in dynamicPredicates) {
                it.seal()
            }
        }
    }

    override val exportedPredicates: Map<ClauseIndicator, PrologCallable> = allDeclaredPredicates
        .filter { it.key in exportedPredicateIndicators }

    private val importLookupCache: Map<ClauseIndicator, PrologCallable> = mutableMapOf<ClauseIndicator, PrologCallable>().also { importLookupCache ->
        imports.asSequence()
            .forEach {
                when (it) {
                    is FullModuleImport -> importLookupCache.putAll(it.module.exportedPredicates)
                    is PartialModuleImport -> importLookupCache.putAll(it.imports)
                }
            }
    }

    override fun findImport(indicator: ClauseIndicator): PrologCallable? = importLookupCache[indicator]

    override fun deriveScopedProofSearchContext(deriveFrom: ProofSearchContext): ProofSearchContext {
        if (deriveFrom is ModuleScopeProofSearchContext) {
            return if (deriveFrom.module == this) deriveFrom else {
                ModuleScopeProofSearchContext(deriveFrom.invokedFrom, this, this.allDeclaredPredicates)
            }
        }

        return ModuleScopeProofSearchContext(deriveFrom, this, this.allDeclaredPredicates)
    }
}