package com.github.prologdb.parser

import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.*
import java.util.concurrent.ConcurrentHashMap

/**
 * Useful for providing modules from source code at runtime (e.g. from user input).
 */
class PredefinedSourceModuleLoader(
    private val sourceFileVisitorSupplier: (ModuleReference, PrologRuntimeEnvironment) -> SourceFileVisitor<Module> = { _, runtime -> DefaultModuleSourceFileVisitor(runtime) },
    private val parser: PrologParser = PrologParser()
) : ModuleLoader {
    private val moduleSources = ConcurrentHashMap<ModuleReference, ModuleSource>()

    @JvmOverloads
    fun registerModule(
        reference: ModuleReference,
        source: String,
        sourceUnit: SourceUnit = SourceUnit("module $reference"),
        implicitDeclaration: ModuleDeclaration = ModuleDeclaration(reference.moduleName)
    ) {
        val existing = moduleSources.putIfAbsent(reference, ModuleSource(source, sourceUnit, implicitDeclaration))
        if (existing != null) {
            throw IllegalStateException("A module $reference is already registered")
        }
    }

    override fun initiateLoading(
        reference: ModuleReference,
        runtime: PrologRuntimeEnvironment
    ): ModuleLoader.PrimedStage {
        val source = moduleSources[reference] ?: throw ModuleNotFoundException(reference)
        val sourceFileVisitor = sourceFileVisitorSupplier(reference, runtime)
        val lexer = Lexer(source.unit, LineEndingNormalizer(source.source.iterator()))

        return parser.parseSourceFile(lexer, sourceFileVisitor, source.implicitDeclaration)
    }

    private class ModuleSource(
        val source: String,
        val unit: SourceUnit,
        val implicitDeclaration: ModuleDeclaration?
    )
}