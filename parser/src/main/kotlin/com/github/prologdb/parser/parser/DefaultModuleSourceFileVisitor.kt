package com.github.prologdb.parser.parser

import com.github.prologdb.parser.*
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.ASTModule
import com.github.prologdb.runtime.module.ModuleDeclaration
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleReference

open class DefaultModuleSourceFileVisitor @JvmOverloads constructor(
    forRuntime: PrologRuntimeEnvironment,
    /**
     * These modules will be imported by default (no explicit import necessary)
     */
    defaultImports: Set<ModuleImport.Full> = DEFAULT_IMPORTS
) : AbstractSourceFileVisitor<ASTModule>(forRuntime) {
    private val clauses = mutableListOf<Clause>()
    private val dynamics = mutableSetOf<ClauseIndicator>()
    private val moduleTransparents = mutableSetOf<ClauseIndicator>()
    private val imports = mutableListOf<ModuleImport>()
    private val defaultImports: MutableSet<ModuleImport> = defaultImports.toMutableSet()

    override fun visitModuleDeclaration(declaration: ModuleDeclaration, location: SourceLocation) {
        super.visitModuleDeclaration(declaration, location)

        // this has to wait until here to prevent a stack overflow on cyclic imports
        for (import in defaultImports) {
            val importedModuleDeclaration = forRuntime.assureModulePrimed(import.moduleReference)
            if (importedModuleDeclaration.exportedOperators.allOperators.any()) {
                throw ParseException.ofSingle(
                    SemanticError(
                        "Module ${import.moduleReference} imported by default exports operators. This is not allowed because it would make for fuzzy and unintuitive semantics around these operators.",
                        SourceLocation.EOF
                    ),
                )
            }
        }
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

    override fun visitImport(import: ModuleImport, location: SourceLocation): Collection<Reporting> {
        val wasDefault = defaultImports.removeIf { it.moduleReference == import.moduleReference }
        imports.add(import)

        if (wasDefault) {
            // cannot have operators, so no need to handle
            return emptyList()
        }

        val reportings = mutableListOf<Reporting>()
        val importedDeclaration = forRuntime.assureModulePrimed(import.moduleReference)
        when (import) {
            is ModuleImport.Full -> {
                operators.include(importedDeclaration.exportedOperators)
            }
            is ModuleImport.Selective -> {
                for (operatorImport in import.operators) {
                    val operatorsToImport = importedDeclaration.exportedOperators.allOperators.filter { operatorImport.matches(it) }
                    if (operatorsToImport.isEmpty()) {
                        reportings.add(SemanticWarning(
                            "No exported operator of module ${importedDeclaration.moduleName} matches the import $operatorImport",
                            location,
                        ))
                    }
                    operatorsToImport.forEach(operators::defineOperator)
                }
            }
            is ModuleImport.Except -> {
                val operatorsToImport = importedDeclaration.exportedOperators.allOperators.toMutableList()
                for (operatorExclusion in import.excludedOperators) {
                    val anyRemoved = operatorsToImport.removeIf { operatorExclusion.matches(it) }
                    if (!anyRemoved) {
                        reportings.add(SemanticWarning(
                            "No exported operator of module ${importedDeclaration.moduleName} matches the exclusion $operatorExclusion",
                            location,
                        ))
                    }
                }
                operatorsToImport.forEach(operators::defineOperator)
            }
        }

        return reportings
    }

    override fun visitClause(clause: Clause, location: SourceLocation): Collection<Reporting> {
        clauses.add(clause)
        return emptyList()
    }

    override fun buildResult(): ParseResult<ASTModule> {
        val moduleDeclaration = this.moduleDeclaration
        check(moduleDeclaration != null) { "No module declaration given from parser" }

        return ParseResult(
            ASTModule(
                moduleDeclaration,
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
            ModuleImport.Full(ModuleReference("library", "dicts")),
            ModuleImport.Full(ModuleReference("library", "solution_sequences")),
        )
    }
}
