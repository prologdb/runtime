package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.module.ModuleScopeProofSearchContext
import com.github.prologdb.runtime.module.NoopModuleLoader
import com.github.prologdb.runtime.proofsearch.Authorization
import com.github.prologdb.runtime.proofsearch.PrologCallable
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.proofsearch.ReadWriteAuthorization
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import java.util.Collections
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

/**
 * The environment for one **instance** of a prolog program.
 */
class PrologRuntimeEnvironment(
    val rootModule: Module,
    private val moduleLoader: ModuleLoader = NoopModuleLoader
) {
    /**
     * Maps module names to the loaded [Module]s.
     */
    private val _loadedModules: MutableMap<String, Module> = ConcurrentHashMap()
    val loadedModules: Map<String, Module> = Collections.unmodifiableMap(_loadedModules)
    private val moduleLookupTables: MutableMap<String, Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>>> = ConcurrentHashMap()
    private val moduleLoadingMutex = Any()

    init {
        assureModuleLoaded(rootModule)
    }

    // TODO: check for indicator collisions in module exports

    private fun assureModuleLoaded(reference: ModuleReference, assureLookups: Boolean = true): Module {
        synchronized(moduleLoadingMutex) {
            val existingModule = _loadedModules[reference.moduleName]
            if (existingModule != null) return existingModule

            return assureModuleLoaded(moduleLoader.load(reference), assureLookups)
        }
    }

    private fun assureModuleLoaded(module: Module, assureLookups: Boolean = true): Module {
        synchronized(moduleLoadingMutex) {
            val existingModule = _loadedModules.putIfAbsent(module.name, module)
            if (existingModule != null) {
                return module
            }

            val transitiveLoads = loadImports(module)
            if (assureLookups) {
                (transitiveLoads + module).forEach { loadedModule ->
                    moduleLookupTables.computeIfAbsent(loadedModule.name) { _ ->
                        buildModuleLookupTable(loadedModule)
                    }
                }
            }

            return module
        }
    }

    /**
     * @return the module names loaded (including transitives)
     */
    private fun loadImports(module: Module): Set<Module> {
        return module.imports
            .map { import ->
                try {
                    assureModuleLoaded(import.moduleReference, false)
                } catch (ex: ModuleNotFoundException) {
                    ex.addLoadChainElement("imported by module ${module.name}")
                    throw ex
                }
            }
            .toSet()
    }

    fun newProofSearchContext(authorization: Authorization = ReadWriteAuthorization): ProofSearchContext {
        return ModuleScopeProofSearchContext(
            rootModule,
            this,
            moduleLookupTables.getValue(rootModule.name),
            UUID.randomUUID(),
            RandomVariableScope(),
            authorization
        )
    }

    fun deriveProofSearchContextForModule(deriveFrom: ProofSearchContext, moduleName: String): ProofSearchContext {
        if (deriveFrom is ModuleScopeProofSearchContext && deriveFrom.module.name == moduleName) {
            return deriveFrom
        }

        val module = _loadedModules[moduleName] ?: throw PrologRuntimeException("Module $moduleName is not loaded.")
        return ModuleScopeProofSearchContext(
            module,
            this,
            moduleLookupTables.getValue(module.name),
            deriveFrom.principal,
            deriveFrom.randomVariableScope,
            deriveFrom.authorization
        )
    }

    private fun buildModuleLookupTable(module: Module): Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> {
        return mutableMapOf<ClauseIndicator, Pair<ModuleReference, PrologCallable>>().also { importLookupCache ->
            module.imports.asSequence()
                .forEach { import ->
                    val referencedModule = _loadedModules[import.moduleReference.moduleName]
                        ?: throw PrologRuntimeException("Imported module ${import.moduleReference} not loaded in proof search context")

                    val visiblePredicates: Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> = when (import) {
                        is ModuleImport.Full      -> referencedModule.exportedPredicates
                            .mapValues { (_, callable) ->
                                Pair(import.moduleReference, callable)
                            }
                        is ModuleImport.Selective -> import.imports
                            .map { (exportedIndicator, alias) ->
                                val callable = referencedModule.exportedPredicates[exportedIndicator]
                                    ?: throw PrologRuntimeException("Predicate $exportedIndicator not exported by module ${import.moduleReference}")
                                if (exportedIndicator.functor == alias) {
                                    exportedIndicator to Pair(import.moduleReference, callable)
                                } else {
                                    ClauseIndicator.of(alias, exportedIndicator.arity) to Pair(import.moduleReference, callable)
                                }
                            }
                            .toMap()

                        is ModuleImport.Except    -> referencedModule.exportedPredicates
                            .filterKeys { it !in import.excluded }
                            .mapValues { (_, callable) ->
                                Pair(import.moduleReference, callable)
                            }
                    }

                    importLookupCache.putAll(visiblePredicates)
                }
        }
    }

    @JvmOverloads
    fun fulfill(goal: Query, authorization: Authorization = ReadWriteAuthorization): LazySequence<Unification> {
        val psc = newProofSearchContext(authorization)

        return buildLazySequence(psc.principal) {
            psc.fulfillAttach(this, goal, VariableBucket())
        }
    }
}
