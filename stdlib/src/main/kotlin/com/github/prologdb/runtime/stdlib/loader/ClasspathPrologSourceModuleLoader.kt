package com.github.prologdb.runtime.stdlib.loader

import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.*
import java.util.*

class ClasspathPrologSourceModuleLoader(
    private val sourceFileVisitorSupplier: (ModuleReference, PrologRuntimeEnvironment) -> SourceFileVisitor<Module> = { _, runtime -> DefaultModuleSourceFileVisitor(runtime) },
    private val parser: PrologParser = PrologParser(),
    private val classLoader: ClassLoader = ClassLoader.getSystemClassLoader(),
    private val moduleReferenceToClasspathPath: (ModuleReference) -> String = { reference ->
        "${reference.pathAlias}/${reference.moduleName}.pl"
    },
    private val charset: Charset = Charsets.UTF_8,
) : ModuleLoader {
    override fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment): PrologParser.PrimedStage {
        val classpathPath = moduleReferenceToClasspathPath(reference)
        val sourceFileUrl = classLoader.getResource(classpathPath)
            ?: throw ModuleNotFoundException(
                reference,
                cause = MissingResourceException(
                    "Cannot find $classpathPath in the classpath",
                    classpathPath,
                    ""
                )
            )

        val sourceText = sourceFileUrl.readText(charset)
        val sourceUnit = SourceUnit(sourceFileUrl.toString())
        return parser.parseSourceFile(
            Lexer(sourceUnit, LineEndingNormalizer(sourceText.iterator())),
            sourceFileVisitorSupplier(reference, runtime),
            ModuleDeclaration(reference.moduleName),
        )
    }
}
