package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologRuntimeEnvironment
import java.util.concurrent.ConcurrentHashMap

/**
 * A [ModuleLoader] that just provides modules that are predefined but were
 * loaded/defined somewhere else.
 *
 * Only use this class if you know what you are doing. For anything that relates
 * to standard prolog, use the standard library modules from stdlib.
 */
class PredefinedModuleLoader : ModuleLoader {
    private val modules: MutableMap<ModuleReference, Module> = ConcurrentHashMap()

    fun registerModule(pathAlias: String, module: Module) {
        val reference = ModuleReference(pathAlias, module.declaration.moduleName)

        if (modules.putIfAbsent(reference, module) != null) {
            throw IllegalStateException("Module ${reference.moduleName} already registered")
        }
    }

    override fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment): ModuleLoader.PrimedStage {
        val module = modules[reference] ?: throw ModuleNotFoundException(reference)
        val parsedStage = object : ModuleLoader.ParsedStage {
            override val module = module
        }

        return object : ModuleLoader.PrimedStage {
            override val declaration = module.declaration

            override fun proceed() = parsedStage
        }
    }
}
