package com.github.prologdb.parser.parser

import com.github.prologdb.parser.ModuleDeclaration
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.SemanticInfo
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.module.ASTModule
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.term.CompoundTerm

open class DefaultModuleSourceFileVisitor @JvmOverloads constructor(
    /**
     * If not-null: makes the module declaration implicit, the source file need not declare a module.
     * It becomes an error for the source file to contain a `:- module` directive.
     */
    protected val implicitModule: ModuleDeclaration? = null,

    /**
     * These modules will be imported by default (no explicit import necessary)
     */
    defaultImports: Set<ModuleImport.Full> = DEFAULT_IMPORTS
) : AbstractSourceFileVisitor<ASTModule>() {
    private val clauses = mutableListOf<Clause>()
    private val dynamics = mutableSetOf<ClauseIndicator>()
    private val moduleTransparents = mutableSetOf<ClauseIndicator>()
    private var visitedModuleDeclaration: ModuleDeclaration? = null
    private val imports = mutableListOf<ModuleImport>()
    private val defaultImports: MutableSet<ModuleImport> = defaultImports.toMutableSet()

    private val moduleDeclared: Boolean get() = implicitModule != null || visitedModuleDeclaration != null

    override fun visitDirective(command: CompoundTerm): Collection<Reporting> {
        val superResult = super.visitDirective(command)

        if (!moduleDeclared) {
            if (command.functor != "module" || command.arity !in 1..2) {
                return superResult + listOf(moduleNotFirstStatementInFileError(command.sourceInformation as SourceLocation))
            }
        }

        return superResult
    }

    override fun visitDynamicDeclaration(
        clauseIndicator: ClauseIndicator,
        location: SourceLocation
    ): Collection<Reporting> {
        if (clauseIndicator in dynamics) {
            return listOf(SemanticInfo(
                "Predicate $clauseIndicator has been declared dynamic multiple times.",
                location
            ))
        }

        dynamics.add(clauseIndicator)
        return emptyList()
    }

    override fun visitModuleTransparentDeclaration(clauseIndicator: ClauseIndicator, location: SourceLocation): Collection<Reporting> {
        if (clauseIndicator in moduleTransparents) {
            return listOf(SemanticInfo(
                "Predicate $clauseIndicator has been declared module transparent multiple times.",
                location
            ))
        }

        moduleTransparents.add(clauseIndicator)
        return emptyList()
    }

    override fun visitModuleDeclaration(
        declaration: ModuleDeclaration,
        location: SourceLocation
    ): Collection<Reporting> {
        if (moduleDeclared) {
            if (implicitModule == null) {
                return listOf(
                    SemanticError(
                        "Module already declared as ${visitedModuleDeclaration!!.moduleName}",
                        location
                    )
                )
            } else {
                return listOf(SemanticError(
                    "Module is implicitly declared as ${implicitModule.moduleName}, cannot declare it explicitly",
                    location
                ))
            }
        }

        visitedModuleDeclaration = declaration
        return emptyList()
    }

    override fun visitImport(import: ModuleImport, location: SourceLocation): Collection<Reporting> {
        if (import !in defaultImports) {
            defaultImports.removeIf { it.moduleReference == import.moduleReference }
            imports.add(import)
        }

        return if (!moduleDeclared) {
            listOf(moduleNotFirstStatementInFileError(location))
        } else {
            emptyList()
        }
    }

    override fun visitClause(clause: Clause, location: SourceLocation): Collection<Reporting> {
        clauses.add(clause)

        return if (!moduleDeclared) {
            listOf(moduleNotFirstStatementInFileError(location))
        } else {
            emptyList()
        }
    }

    override fun buildResult(): ParseResult<ASTModule> {
        val moduleDeclaration = this.visitedModuleDeclaration ?: this.implicitModule ?: return ParseResult(
            null,
            ParseResultCertainty.MATCHED,
            setOf(SemanticError("Missing module declaration", SourceLocation.EOF))
        )

        return ParseResult(
            ASTModule(
                moduleDeclaration.moduleName,
                defaultImports.toList() + imports,
                clauses,
                dynamics,
                moduleTransparents,
                moduleDeclaration.exportedPredicates
                    ?: clauses.map { ClauseIndicator.of(it) }.toSet(),
                operators
            ),
            ParseResultCertainty.MATCHED,
            emptySet()
        )
    }

    private fun moduleNotFirstStatementInFileError(location: SourceLocation) = SemanticError(
        "The module/1 directive must be the first statement in the file",
        location
    )

    companion object {
        /**
         * These are modules containing the basic built-ins that are callable without import in other prologs.
         */
        val DEFAULT_IMPORTS: Set<ModuleImport.Full> = setOf(
            ModuleImport.Full(ModuleReference("essential", "\$equality")),
            ModuleImport.Full(ModuleReference("essential", "\$clauses")),
            ModuleImport.Full(ModuleReference("essential", "\$dynamic")),
            ModuleImport.Full(ModuleReference("essential", "\$math")),
            ModuleImport.Full(ModuleReference("essential", "\$strings")),
            ModuleImport.Full(ModuleReference("essential", "\$comparison")),
            ModuleImport.Full(ModuleReference("essential", "\$typesafety")),
            ModuleImport.Full(ModuleReference("library", "lists")),
            ModuleImport.Full(ModuleReference("library", "sort")),
            ModuleImport.Full(ModuleReference("library", "dicts"))
        )
    }
}
