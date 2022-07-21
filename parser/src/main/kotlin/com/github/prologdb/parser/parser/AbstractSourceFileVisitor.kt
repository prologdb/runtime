package com.github.prologdb.parser.parser

import com.github.prologdb.parser.ParseException
import com.github.prologdb.runtime.module.ModuleDeclaration
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.util.DefaultOperatorRegistry
import com.github.prologdb.runtime.util.MutableOperatorRegistry
import com.github.prologdb.runtime.util.OperatorDefinition

/**
 * Contains some code for handling very common things in source files
 * that most implementations of [SourceFileVisitor] will likely benefit from:
 *
 * * handles the `op/3` directive entirely
 * * provides convenience methods for these directives (e.g. [visitImport]):
 *     * `dynamic/1`
 *     * `module/1`
 *     * `module/2`
 *     * `use_module/1`
 *     * `use_module/2`
 *     * `op/3`
 */
abstract class AbstractSourceFileVisitor<Result : Any>(
    val forRuntime: PrologRuntimeEnvironment,
) : SourceFileVisitor<Result> {
    protected var moduleDeclaration: ModuleDeclaration? = null

    override val operators: MutableOperatorRegistry = DefaultOperatorRegistry().apply {
        include(ISOOpsOperatorRegistry)
    }

    override fun visitModuleDeclaration(declaration: ModuleDeclaration, location: SourceLocation) {
        check(this.moduleDeclaration == null) { "There can only be a single module declarations in a source file." }

        this.moduleDeclaration = declaration

        operators.include(declaration.exportedOperators)
    }

    override fun visitDirective(command: CompoundTerm): Collection<Reporting> {
        when(command.arity) {
            1 -> when (command.functor) {
                "dynamic" -> return convertAndVisit(command.arguments[0], parser::parseIdiomaticClauseIndicator, this::visitDynamicDeclaration)
                "module" -> return setOf(SemanticError(
                    "Cannot redeclare the module",
                    command.sourceInformation as SourceLocation
                ))
                "use_module" -> return convertAndVisit(command, this::convertModuleImport, this::visitImport)
                "module_transparent" -> return convertAndVisit(command.arguments[0],
                    parser::parseIdiomaticClauseIndicator,
                    this::visitModuleTransparentDeclaration)
            }
            2 -> when (command.functor) {
                "module" -> return setOf(SemanticError(
                    "Cannot redeclare the module",
                    command.sourceInformation as SourceLocation
                ))
                "use_module" -> return convertAndVisit(command, this::convertModuleImport, this::visitImport)
            }
            3 -> when(command.functor) {
                "op" -> return convertAndVisit(command, this::convertOperatorDefinition, this::visitOperatorDefinition)
            }
        }

        return listOf(SemanticError(
            "Directive ${ClauseIndicator.of(command)} is not defined.",
            command.sourceInformation as SourceLocation
        ))
    }

    override fun tryParseModuleDeclaration(firstTermOfFile: Term): ParseResult<ModuleDeclaration>? {
        if (firstTermOfFile !is CompoundTerm || firstTermOfFile.functor != ":-" || firstTermOfFile.arity != 1) {
            // not a directive
            return null
        }

        val directive = firstTermOfFile.arguments.single()

        if (directive !is CompoundTerm || directive.functor != "module") {
            return null
        }

        return parser.parseModuleDeclaration(directive)
    }

    /**
     * Called by [visitDirective] if it finds a operator definition. The default behaviour
     * is to just set/override the operator using [DefaultOperatorRegistry.defineOperator] on [operators].
     */
    protected open fun visitOperatorDefinition(definition: OperatorDefinition, location: SourceLocation): Collection<Reporting> {
        operators.defineOperator(definition)
        return emptyList()
    }

    protected abstract fun visitDynamicDeclaration(clauseIndicator: ClauseIndicator, location: SourceLocation): Collection<Reporting>

    protected abstract fun visitModuleTransparentDeclaration(
        clauseIndicator: ClauseIndicator,
        location: SourceLocation
    ): Collection<Reporting>

    protected abstract fun visitImport(import: ModuleImport, location: SourceLocation): Collection<Reporting>

    /**
     * @see PrologParser.parseOperatorDefinition
     * Override only to support alternate syntax for operator definitions.
     */
    protected open fun convertOperatorDefinition(opDefinitionAST: CompoundTerm): ParseResult<OperatorDefinition> {
        return parser.parseOperatorDefinition(opDefinitionAST)
    }

    /**
     * @see PrologParser.parseModuleDeclaration
     * Override only to support alternate syntax for module declarations.
     */
    protected open fun convertModuleDeclaration(moduleDeclarationAST: CompoundTerm): ParseResult<ModuleDeclaration> {
        return parser.parseModuleDeclaration(moduleDeclarationAST)
    }

    /**
     * @see PrologParser.parseModuleImport
     * Override only to support alternate syntax for imports.
     */
    protected open fun convertModuleImport(importAST: CompoundTerm) : ParseResult<ModuleImport> {
        return parser.parseModuleImport(importAST)
    }

    companion object {
        protected val parser = PrologParser()
        protected fun <I : Term, R : Any> convertAndVisit(input: I, converter: (I) -> ParseResult<R>, visitor: (R, SourceLocation) -> Collection<Reporting>): Collection<Reporting> {
            val result = converter(input)
            if (result.item == null) {
                return result.reportings
            }

            return result.reportings + visitor(result.item, input.sourceInformation as SourceLocation)
        }
    }
}
