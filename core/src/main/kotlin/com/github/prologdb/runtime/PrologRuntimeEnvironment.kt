package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.runtime.knowledge.Authorization
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.ReadWriteAuthorization
import com.github.prologdb.runtime.knowledge.library.Module
import com.github.prologdb.runtime.knowledge.library.ModuleLoader
import com.github.prologdb.runtime.knowledge.library.ModuleReference
import com.github.prologdb.runtime.knowledge.library.NativeLibraryLoader
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import java.util.*
import java.util.concurrent.ConcurrentHashMap

/**
 * The environment for one **instance** of a prolog program.
 */
class PrologRuntimeEnvironment(
    val rootModule: Module,
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

    @JvmOverloads
    fun fulfill(goal: Query, authorization: Authorization = ReadWriteAuthorization): LazySequence<Unification> {
        val psc = newProofSearchContext()
        return buildLazySequence(psc.principal) {
            psc.fulfillAttach(this, goal, VariableBucket())
        }
    }
}
