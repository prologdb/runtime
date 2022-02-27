package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.PrologRuntimeEnvironment.Companion.buildModuleLookupTable
import com.github.prologdb.runtime.module.InvalidImportException
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleNotLoadedException
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

interface PrologRuntimeEnvironment {
    val rootModule: Module
    val loadedModules: Map<String, Module>
    fun newProofSearchContext(authorization: Authorization = ReadWriteAuthorization): ProofSearchContext
    fun deriveProofSearchContextForModule(deriveFrom: ProofSearchContext, moduleName: String): ProofSearchContext

    companion object {
        /**
         * @return a table that, considering all imports, maps unscoped predicates to the actual implementations
         */
        @JvmStatic
        fun buildModuleLookupTable(allModules: Map<String, Module>, forModule: Module): Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> {
            return mutableMapOf<ClauseIndicator, Pair<ModuleReference, PrologCallable>>().also { importLookupCache ->
                forModule.imports.asSequence()
                    .forEach { import ->
                        val referencedModule = allModules[import.moduleReference.moduleName]
                            ?: throw ModuleNotLoadedException("Imported module ${import.moduleReference} not loaded in proof search context")

                        val visiblePredicates: Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>> = when (import) {
                            is ModuleImport.Full      -> referencedModule.exportedPredicates
                                .mapValues { (_, callable) ->
                                    Pair(import.moduleReference, callable)
                                }
                            is ModuleImport.Selective -> import.imports
                                .map { (exportedIndicator, alias) ->
                                    val callable = referencedModule.exportedPredicates[exportedIndicator]
                                        ?: throw InvalidImportException(forModule.name, import, "Predicate $exportedIndicator not exported by module ${import.moduleReference}")

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
    }
}

/**
 * The environment for one **instance** of a prolog program.
 */
open class DefaultPrologRuntimeEnvironment(
    override val rootModule: Module,
    protected val moduleLoader: ModuleLoader = NoopModuleLoader
) : PrologRuntimeEnvironment {
    /**
     * Maps module names to the loaded [Module]s.
     */
    private val _loadedModules: MutableMap<String, Module> = ConcurrentHashMap()
    override val loadedModules: Map<String, Module> = Collections.unmodifiableMap(_loadedModules)
    protected val moduleLookupTables: MutableMap<String, Map<ClauseIndicator, Pair<ModuleReference, PrologCallable>>> = ConcurrentHashMap()
    private val moduleLoadingMutex = Any()

    init {
        assureModuleLoaded(rootModule)
    }

    // TODO: check for indicator collisions in module exports

    protected fun assureModuleLoaded(reference: ModuleReference): Module {
        synchronized(moduleLoadingMutex) {
            val existingModule = _loadedModules[reference.moduleName]
            if (existingModule != null) return existingModule

            return assureModuleLoaded(moduleLoader.load(reference))
        }
    }

    protected fun assureModuleLoaded(module: Module): Module {
        synchronized(moduleLoadingMutex) {
            val existingModule = _loadedModules.putIfAbsent(module.name, module)
            if (existingModule != null) {
                return module
            }

            val transitiveLoads = loadImports(module)
            (transitiveLoads + module).forEach { loadedModule ->
                moduleLookupTables.computeIfAbsent(loadedModule.name) { _ ->
                    buildModuleLookupTable(loadedModules, loadedModule)
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
                    assureModuleLoaded(import.moduleReference)
                } catch (ex: ModuleNotFoundException) {
                    ex.addLoadChainElement("imported by module ${module.name}")
                    throw ex
                }
            }
            .toSet()
    }

    override fun newProofSearchContext(authorization: Authorization): ProofSearchContext {
        return ModuleScopeProofSearchContext(
            rootModule,
            this,
            moduleLookupTables.getValue(rootModule.name),
            UUID.randomUUID(),
            RandomVariableScope(),
            authorization
        )
    }

    override fun deriveProofSearchContextForModule(deriveFrom: ProofSearchContext, moduleName: String): ProofSearchContext {
        if (deriveFrom is ModuleScopeProofSearchContext && deriveFrom.module.name == moduleName) {
            return deriveFrom
        }

        val module = _loadedModules[moduleName] ?: throw ModuleNotLoadedException(moduleName)
        return ModuleScopeProofSearchContext(
            module,
            this,
            moduleLookupTables.getValue(module.name),
            deriveFrom.principal,
            deriveFrom.randomVariableScope,
            deriveFrom.authorization
        )
    }

    @JvmOverloads
    fun fulfill(goal: Query, authorization: Authorization = ReadWriteAuthorization): LazySequence<Unification> {
        val psc = newProofSearchContext(authorization)

        return buildLazySequence(psc.principal) {
            psc.fulfillAttach(this, goal, VariableBucket())
        }
    }
}
