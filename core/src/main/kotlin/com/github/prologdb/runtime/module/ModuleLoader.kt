package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.builtin.EssentialComparisonModule
import com.github.prologdb.runtime.builtin.EssentialEqualityModule
import com.github.prologdb.runtime.builtin.dicts.DictsModule
import com.github.prologdb.runtime.builtin.essential.EssentialTypeSafetyModule
import com.github.prologdb.runtime.builtin.essential.dynamic.EssentialDynamicModule
import com.github.prologdb.runtime.builtin.essential.math.EssentialMathModule
import com.github.prologdb.runtime.builtin.essential.string.EssentialStringsModule
import com.github.prologdb.runtime.builtin.lists.ListsModule
import com.github.prologdb.runtime.builtin.sort.SortModule
import com.github.prologdb.runtime.module.NativeLibraryLoader.Companion.withCoreLibraries
import java.util.concurrent.ConcurrentHashMap

interface ModuleLoader {
    fun load(reference: ModuleReference): Module
}

/**
 * Throws an exception when attempting to load any module.
 */
object NoopModuleLoader : ModuleLoader {
    override fun load(reference: ModuleReference): Module {
        throw ModuleNotFoundException(reference)
    }
}

class NativeLibraryLoader
/**
 * Only use this constructor if you know what you are doing. For anything that relates to standard-prolog,
 * [withCoreLibraries] is a better choice.
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

    companion object {
        /**
         * A loader that contains references to these native built-in modules:
         *
         * * `essential($dicts)`
         * * `essential($dynamics)`
         * * `essential($math)`
         * * `essential($strings)`
         * * `essential($comparison)`
         * * `essential($equality)`
         * * `essential($typesafety)`
         * * `library(lists)`
         * * `library(sort)`
         * * `library(dicts)`
         */
        @JvmStatic
        @JvmOverloads
        fun withCoreLibraries(delegate: ModuleLoader = NoopModuleLoader): ModuleLoader {
            val loader = NativeLibraryLoader(delegate)

            loader.registerModule("essential", EssentialDynamicModule)
            loader.registerModule("essential", EssentialMathModule)
            loader.registerModule("essential", EssentialStringsModule)
            loader.registerModule("essential", EssentialComparisonModule)
            loader.registerModule("essential", EssentialEqualityModule)
            loader.registerModule("essential", EssentialTypeSafetyModule)

            loader.registerModule("library", ListsModule)
            loader.registerModule("library", SortModule)
            loader.registerModule("library", DictsModule)

            return loader
        }
    }
}
