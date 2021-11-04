package com.github.prologdb.runtime.module

import java.util.concurrent.ConcurrentHashMap

/**
 * A [ModuleLoader] for modules implemented in Kotlin.
 */
class NativeLibraryLoader
/**
 * Only use this constructor if you know what you are doing. For anything that relates to standard-prolog,
 * use the standard library modules from stdlib.
 */
constructor(private val delegate: ModuleLoader = NoopModuleLoader) : ModuleLoader {
    private val nativeLibraries: MutableMap<ModuleReference, Module> = ConcurrentHashMap()

    fun registerModule(pathAlias: String, module: Module) {
        val reference = ModuleReference(pathAlias, module.name)

        if (nativeLibraries.putIfAbsent(reference, module) != null) {
            throw IllegalStateException("Module ${reference.moduleName} already registered")
        }
    }

    override fun load(reference: ModuleReference): Module {
        return nativeLibraries[reference] ?: delegate.load(reference)
    }
}
