package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.lists.ListsModule
import java.util.concurrent.ConcurrentHashMap

interface ModuleLoader {
    fun load(reference: ModuleReference): Module
}

class NoopModuleLoader : ModuleLoader {
    override fun load(reference: ModuleReference): Module {
        throw PrologRuntimeException("Module $reference not found.")
    }
}

class NativeLibraryLoader(private val delegate: ModuleLoader = NoopModuleLoader()) : ModuleLoader {
    private val nativeLibraries: MutableMap<ModuleReference, Module> = ConcurrentHashMap()

    fun registerModule(reference: ModuleReference, module: Module) {
        if (module.name != reference.moduleName) {
            throw IllegalArgumentException("Module reference does not match module: different names")
        }

        if (nativeLibraries.putIfAbsent(reference, module) != null) {
            throw IllegalStateException("Module ${reference.moduleName} already registered")
        }
    }

    override fun load(reference: ModuleReference): Module {
        return nativeLibraries[reference] ?: delegate.load(reference)
    }

    companion object {
        @JvmStatic
        fun withCoreLibraries(delegate: ModuleLoader = NoopModuleLoader()): ModuleLoader {
            val loader = NativeLibraryLoader(delegate)

            loader.registerModule(ModuleReference("library", "lists"), ListsModule)

            return loader
        }
    }
}
