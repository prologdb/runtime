package com.github.prologdb.runtime

import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.ReadWriteAuthorization
import com.github.prologdb.runtime.knowledge.library.Module
import com.github.prologdb.runtime.knowledge.library.ModuleLoader
import com.github.prologdb.runtime.knowledge.library.ModuleReference
import com.github.prologdb.runtime.knowledge.library.NativeLibraryLoader
import java.util.*
import java.util.concurrent.ConcurrentHashMap

/**
 * The environment for one **instance** of a prolog program.
 */
class PrologRuntimeEnvironment(
    private val rootModule: Module,
    private val moduleLoader: ModuleLoader = NativeLibraryLoader()
) {
    /**
     * Maps module names to the loaded [Modules].
     */
    private val loadedModules: MutableMap<String, Module> = ConcurrentHashMap()
    private val moduleLoadingMutex = Any()

    init {
        loadedModules[rootModule.name] = rootModule
        loadImports(rootModule)
    }

    // TODO: check for indicator collisions in module exports

    private fun assureModuleLoaded(reference: ModuleReference) {
        synchronized(moduleLoadingMutex) {
            val modulePresent = reference.moduleName in loadedModules
            if (modulePresent) return

            val module = loadedModules.computeIfAbsent(reference.moduleName) { moduleLoader.load(reference) }
            loadImports(module)
        }
    }

    private fun loadImports(module: Module) {
        module.imports.forEach { import ->
            assureModuleLoaded(import.moduleReference)
        }
    }

    fun newProofSearchContext(): ProofSearchContext {
        return rootModule.createProofSearchContext(
            UUID.randomUUID(),
            RandomVariableScope(),
            ReadWriteAuthorization,
            loadedModules
        )
    }
}
