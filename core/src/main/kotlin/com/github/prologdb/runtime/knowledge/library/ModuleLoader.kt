package com.github.prologdb.runtime.knowledge.library

import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.ComparisonModule
import com.github.prologdb.runtime.builtin.EqualityModule
import com.github.prologdb.runtime.builtin.TypeSafetyModule
import com.github.prologdb.runtime.builtin.dict.DictModule
import com.github.prologdb.runtime.builtin.dynamic.DynamicsModule
import com.github.prologdb.runtime.builtin.lists.ListsModule
import com.github.prologdb.runtime.builtin.math.MathModule
import com.github.prologdb.runtime.builtin.string.StringsModule
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

    fun registerModule(pathAlias: String, module: Module) {
        val reference = ModuleReference(pathAlias, module.name)

        if (nativeLibraries.putIfAbsent(reference, module) != null) {
            throw IllegalStateException("Module ${reference.moduleName} already registered")
        }
    }

    override fun load(reference: ModuleReference): Module {
        return nativeLibraries[reference] ?: delegate.load(reference)
    }

    companion object {
        @JvmStatic
        @JvmOverloads
        fun withCoreLibraries(delegate: ModuleLoader = NoopModuleLoader()): ModuleLoader {
            val loader = NativeLibraryLoader(delegate)

            loader.registerModule("library", DictModule)
            loader.registerModule("library", DynamicsModule)
            loader.registerModule("library", ListsModule)
            loader.registerModule("library", MathModule)
            loader.registerModule("library", StringsModule)
            loader.registerModule("library", ComparisonModule)
            loader.registerModule("library", EqualityModule)
            loader.registerModule("library", TypeSafetyModule)

            return loader
        }
    }
}
