package com.github.prologdb.runtime.stdlib.loader

import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleReference
import java.nio.file.Path
import java.nio.file.Paths

class ClasspathPrologSourceModuleLoader(
    private val sourceFileVisitorSupplier: (ModuleReference) -> SourceFileVisitor<Module> = { _ -> DefaultModuleSourceFileVisitor() },
    private val parser: PrologParser = PrologParser(),
    private val classLoader: ClassLoader = ClassLoader.getSystemClassLoader(),
    private val moduleReferenceToClasspathPath: (ModuleReference) -> Path = { reference ->
        Paths.get(reference.pathAlias).resolve(reference.moduleName + ".pl")
    }
) : ModuleLoader {
    override fun load(reference: ModuleReference): Module {
        val sourceFileUrl = classLoader.getResource(
            moduleReferenceToClasspathPath(reference).toString()
        )
            ?: throw ModuleNotFoundException(reference)

        val sourceText = sourceFileUrl.readText(Charsets.UTF_8)
        val result = parser.parseSourceFile(
            Lexer(
                SourceUnit(sourceFileUrl.toString()),
                LineEndingNormalizer(sourceText.iterator())
            ),
            sourceFileVisitorSupplier(reference)
        )

        result.reportings.firstOrNull()?.let {
            throw PrologRuntimeException("Failed to load module $reference: $it (and ${result.reportings.size - 1} more)")
        }

        val module = result.item!!

        if (module.name != reference.moduleName) {
            throw PrologRuntimeException("Source for module $reference declares a different module name (${module.name}).")
        }

        return module
    }
}
