package com.github.prologdb.runtime.module

import com.github.prologdb.runtime.PrologRuntimeEnvironment
import java.util.ServiceLoader

/**
 * Module loading is a three-step process due to exported operators and cyclic imports.
 * 1. priming: parses only the module declaration from the source to get the module name and exported operators
 * 2. parsing: parses the rest of the source file, considering all imported and local operators
 * 3. linking: now that all modules and their exported predicates are known, predicate invocations can be linked
 *    to the actual predicate, even if there is a cycle in the module import graph.
 *
 * A [ModuleLoader] abstracts all the details of these first two steps and provides an interface through which
 * the [PrologRuntimeEnvironment] can trigger the the steps. The linking is then handled by the [PrologRuntimeEnvironment].
 */
interface ModuleLoader {
    fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment): PrimedStage

    interface PrimedStage {
        val declaration: ModuleDeclaration

        fun proceed(): ParsedStage
    }

    interface ParsedStage {
        val module: Module
    }

    companion object {
        /**
         * Uses the [ServiceLoader] mechanism to find instances of [ModuleLoader] on the classpath. The discovered
         * loaders will be packed into a [CascadingModuleLoader], in the order defined by [comparator].
         * @param comparator defines the priority of the loaders in the combined loader. Sort lower = higher priority
         */
        @JvmOverloads
        fun discoverOnClasspath(
            classLoader: ClassLoader = Thread.currentThread().contextClassLoader,
            comparator: Comparator<ModuleLoader> = Comparator.comparing { it.javaClass.canonicalName },
        ) : ModuleLoader {
            return CascadingModuleLoader(
                ServiceLoader.load(ModuleLoader::class.java, classLoader)
                    .sortedWith(comparator)
            )
        }
    }
}

