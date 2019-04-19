package com.github.prologdb.runtime.knowledge

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification

/**
 * All code declared inside a module only has access to predicates declared in the same module and
 * imported into that module explicitly **but not** to predicates visible in the scope where the
 * module is being imported into. This [ProofSearchContext] ensures that isolation: running code of
 * module within the proper [ModuleScopeProofSearchContext] achieves that behaviour.
 */
class ModuleScopeProofSearchContext(
    /**
     * The [ProofSearchContext] that invoked a predicate from another module which then causes the
     * necessity for this [ProofSearchContext] to exists.
     */
    internal val invokedFrom: ProofSearchContext,

    internal val module: Module,

    /**
     * Predicates declared in [module], including private ones.
     */
    private val modulePredicates: Map<ClauseIndicator, PrologCallable>
) : ProofSearchContext, AbstractProofSearchContext() {
    override val principal = invokedFrom.principal
    override val randomVariableScope = invokedFrom.randomVariableScope
    override val authorization = invokedFrom.authorization
    override val rootAvailableModules: Map<ModuleReference, Module> = invokedFrom.rootAvailableModules

    override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(goal: CompoundTerm, indicator: ClauseIndicator) {
        modulePredicates[indicator]?.let {
            it.fulfill(this, goal, this@ModuleScopeProofSearchContext)
            return
        }

        findImport(indicator)?.let {
            it.fulfill(this, goal, this@ModuleScopeProofSearchContext)
            return
        }
    }

    private val importLookupCache: Map<ClauseIndicator, PrologCallable> = mutableMapOf<ClauseIndicator, PrologCallable>().also { importLookupCache ->
        module.imports.asSequence()
            .forEach { import ->
                val referencedModule = rootAvailableModules[import.moduleReference]
                    ?: throw PrologRuntimeException("Imported module ${import.moduleReference} not loaded in proof search context")

                val visiblePredicates = when (import) {
                    is FullModuleImport -> referencedModule.exportedPredicates
                    is PartialModuleImport -> referencedModule.exportedPredicates.filter { (exportedIndicator, _) ->
                        exportedIndicator in import.imports
                    }
                }

                importLookupCache.putAll(visiblePredicates)
            }
    }

    private fun findImport(indicator: ClauseIndicator): PrologCallable? = importLookupCache[indicator]
}
