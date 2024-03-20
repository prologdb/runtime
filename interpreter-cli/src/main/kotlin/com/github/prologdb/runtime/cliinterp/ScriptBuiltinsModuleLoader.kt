package com.github.prologdb.runtime.cliinterp

import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.stdlib.loader.ClasspathPrologSourceModuleLoader
import com.github.prologdb.runtime.stdlib.loader.NativeCodeSourceFileVisitorDecorator
import kotlin.system.exitProcess

class ScriptBuiltinsModuleLoaderServiceLoaderProxy : ModuleLoader by ScriptBuiltinsModuleLoader

object ScriptBuiltinsModuleLoader : ModuleLoader {
    private val _parser = PrologParser()
    private val classpathPrefix = "com/github/prologdb/runtime/cliinterp/lib"

    private val classPathLoader = ClasspathPrologSourceModuleLoader(
        sourceFileVisitorSupplier = this::getSourceFileVisitor,
        classLoader = ScriptBuiltinsModuleLoader::class.java.classLoader,
        parser = _parser,
        moduleReferenceToClasspathPath = { moduleRef ->
            "$classpathPrefix/${moduleRef.pathAlias}/${moduleRef.moduleName}.pl"
        }
    )

    override fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment): ModuleLoader.PrimedStage {
        try {
            return classPathLoader.initiateLoading(reference, runtime)
        } catch (ex: ModuleNotFoundException) {
            if (reference.moduleName == "io") {
                ex.printStackTrace(System.err)
                exitProcess(1)
            } else throw ex
        }
    }

    private fun getSourceFileVisitor(moduleReference: ModuleReference, runtime: PrologRuntimeEnvironment): SourceFileVisitor<Module> {
        val nativeImplementations = nativeImplementationsByModuleRef[moduleReference.toString()] ?: emptyMap()
        return NativeCodeSourceFileVisitorDecorator(
            DefaultModuleSourceFileVisitor(runtime),
            nativeImplementations,
            _parser
        )
    }

    private val nativeImplementationsByModuleRef: Map<String, Map<ClauseIndicator, Rule>> = mapOf(
        "script(io)" to listOf(
            BuiltinWrite1,
        )
    ).mapValues { (_, nativeCodes) ->
        nativeCodes.associateBy(ClauseIndicator.Companion::of)
    }
}