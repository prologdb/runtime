package com.github.prologdb.runtime.cliinterp

import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleReference
import java.nio.file.Path
import kotlin.io.path.exists
import kotlin.io.path.readText

class FilesystemModuleLoader(
    val libraryPaths: Map<String, Path>,
    val parser: PrologParser = PrologParser(),
    val sourceFileVisitorSupplier: () -> SourceFileVisitor<Module>,
) : ModuleLoader {
    override fun initiateLoading(
        reference: ModuleReference,
        runtime: PrologRuntimeEnvironment,
    ): ModuleLoader.PrimedStage {
        val path = libraryPaths[reference.pathAlias]
            ?: throw ModuleNotFoundException(reference, "path alias ${reference.pathAlias} is not registered")

        val file = path.resolve(reference.moduleName + ".pl").toAbsolutePath().normalize()
        if (!file.exists()) {
            throw ModuleNotFoundException(reference, "File $file does not exist")
        }
        val code = file.readText()
        return parser.parseSourceFile(
            Lexer(SourceUnit(file.toAbsolutePath().toString()), LineEndingNormalizer(code.iterator())),
            sourceFileVisitorSupplier(),
        )
    }
}