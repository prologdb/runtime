package com.github.prologdb.runtime

import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.Principal
import com.github.prologdb.runtime.knowledge.AbstractProofSearchContext
import com.github.prologdb.runtime.knowledge.Authorization
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.ReadWriteAuthorization
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.unification.Unification
import java.util.*
import java.util.concurrent.ConcurrentHashMap

/**
 * The environment for one **instance** of a prolog program.
 */
class PrologRuntimeEnvironment(
    private val rootModule: Module,
    private val moduleLoader: ModuleLoader = NativeLibraryLoader()
) {
    private val loadedModules: MutableMap<ModuleReference, Module> = ConcurrentHashMap()

    // TODO: execute all (nested) imports
    // TODO: handle circular module dependencies
    // TODO: check for indicator collisions in module exports

    private fun assureModuleLoaded(reference: ModuleReference) {
        val module = loadedModules.computeIfAbsent(reference, moduleLoader::load)
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

    private inner class QueryProofSearchContext(
        override val principal: Principal,
        override val randomVariableScope: RandomVariableScope,
        override val authorization: Authorization
    ) : AbstractProofSearchContext() {
        private val rootModuleContext = rootModule.createProofSearchContext(
            principal,
            randomVariableScope,
            authorization,
            loadedModules
        )

        override val rootAvailableModules = loadedModules

        override suspend fun LazySequenceBuilder<Unification>.doInvokePredicate(goal: CompoundTerm, indicator: ClauseIndicator) {
            TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
        }
    }
}
