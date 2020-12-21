package com.github.prologdb.parser.parser

import com.github.prologdb.parser.ModuleDeclaration
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.SyntaxError
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeException
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.module.ExceptModuleImport
import com.github.prologdb.runtime.module.FullModuleImport
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.module.SelectiveModuleImport
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologNumber
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.util.DefaultOperatorRegistry
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType
import kotlin.math.max
import kotlin.math.min

/**
 * Contains some code for handling very common things in source files
 * that most implementations of [SourceFileVisitor] will likely benefit from:
 *
 * * all the operators in [ISOOpsOperatorRegistry] are defined by default
 * * handles the `op/3`` directive entirely
 * * provides convenience methods for these directives (e.g. [visitImport]):
 *     * `dynamic/1`
 *     * `module/1`
 *     * `module/2`
 *     * `use_module/1`
 *     * `use_module/2`
 *     * `op/3`
 */
abstract class AbstractSourceFileVisitor<Result : Any> : SourceFileVisitor<Result> {
    override val operators = DefaultOperatorRegistry().apply { include(ISOOpsOperatorRegistry) }

    override fun visitDirective(command: CompoundTerm): Collection<Reporting> {
        when(command.arity) {
            1 -> when(command.functor) {
                "dynamic" -> return convertAndVisit(command, this::convertIdiomaticClauseIndicator, this::visitDynamicDeclaration)
                "module" -> return convertAndVisit(command, this::convertModuleDeclaration, this::visitModuleDeclaration)
                "use_module" -> return convertAndVisit(command, this::convertModuleImport, this::visitImport)
            }
            2 -> when (command.functor) {
                "module" -> return convertAndVisit(command, this::convertModuleDeclaration, this::visitModuleDeclaration)
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

    /**
     * Called by [visitDirective] if it finds a operator definition. The default behaviour
     * is to just set/override the operator using [DefaultOperatorRegistry.defineOperator] on [operators].
     */
    protected open fun visitOperatorDefinition(definition: OperatorDefinition, location: SourceLocation): Collection<Reporting> {
        operators.defineOperator(definition)
        return emptyList()
    }

    protected abstract fun visitDynamicDeclaration(clauseIndicator: ClauseIndicator, location: SourceLocation): Collection<Reporting>

    protected abstract fun visitModuleDeclaration(declaration: ModuleDeclaration, location: SourceLocation): Collection<Reporting>

    protected abstract fun visitImport(import: ModuleImport, location: SourceLocation): Collection<Reporting>

    /**
     * Converts the AST of a `:- op/3` directive into an [OperatorDefinition].
     * @param opDefinitionAST The instance of `op/3`
     */
    protected open fun convertOperatorDefinition(opDefinitionAST: CompoundTerm): ParseResult<OperatorDefinition> {
        require(opDefinitionAST.arity == 3)

        val precedenceArgument = opDefinitionAST.arguments[0]
        if (precedenceArgument !is PrologNumber || !precedenceArgument.isInteger) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(
                SemanticError("operator precedence must be an integer", precedenceArgument.sourceInformation as SourceLocation)
            ))
        }

        val reportings = mutableSetOf<Reporting>()

        var precedenceAsLong = precedenceArgument.toInteger()
        if (precedenceAsLong < 0 || precedenceAsLong > 1200) {
            reportings.add(SemanticError("operator precedence must be between 0 and 1200 (inclusive)", opDefinitionAST.arguments[0].sourceInformation as SourceLocation))
            precedenceAsLong = min(max(0, precedenceAsLong), 1200)
        }
        val precedence = precedenceAsLong.toShort()

        if (opDefinitionAST.arguments[1] !is Atom) {
            reportings.add(SemanticError("operator type: expected atom but got ${opDefinitionAST.arguments[1].prologTypeName}", opDefinitionAST.arguments[1].sourceInformation as SourceLocation))
            return ParseResult(null, ParseResultCertainty.MATCHED, reportings)
        }

        val typeAsUCString = (opDefinitionAST.arguments[1] as Atom).name.toUpperCase()
        val operatorType = try {
            OperatorType.valueOf(typeAsUCString)
        }
        catch (ex: IllegalArgumentException) {
            reportings.add(SemanticError("${typeAsUCString.toLowerCase()} is not a known operator type", opDefinitionAST.arguments[1].sourceInformation as SourceLocation))
            return ParseResult(null, ParseResultCertainty.MATCHED, reportings)
        }

        if (opDefinitionAST.arguments[2] !is Atom) {
            reportings.add(SemanticError("operator name: expected atom but got ${opDefinitionAST.arguments[2].prologTypeName}", opDefinitionAST.arguments[2].sourceInformation as SourceLocation))
            return ParseResult(null, ParseResultCertainty.MATCHED, reportings)
        }

        return ParseResult(
            OperatorDefinition(precedence, operatorType, (opDefinitionAST.arguments[2] as Atom).name),
            ParseResultCertainty.MATCHED,
            reportings
        )
    }

    /**
     * Converts the AST of a `:- module/1` or `:- module/2` directive into a [ModuleDeclaration].
     * @param moduleDeclarationAST The instance of `module/1` or `module/2`
     */
    protected open fun convertModuleDeclaration(moduleDeclarationAST: CompoundTerm): ParseResult<ModuleDeclaration> {
        val args = moduleDeclarationAST.arguments
        require(args.size in 1..2)

        if (args[0] !is Atom) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(SemanticError(
                "Argument 0 to module/${args.size} must be an atom, got ${args[0].prologTypeName}",
                args[0].sourceInformation as SourceLocation
            )))
        }

        val name = (args[0] as Atom).name
        var exportSelection: Set<ClauseIndicator>? = null

        val reportings = mutableSetOf<Reporting>()
        if (args.size == 2) {
            if (args[1] is PrologList) {
                exportSelection = (args[1] as PrologList).elements
                    .mapNotNull {
                        val indicatorResult = convertIdiomaticClauseIndicator(it)
                        reportings.addAll(indicatorResult.reportings)
                        indicatorResult.item
                    }
                    .toSet()
            } else {
                reportings.add(
                    SemanticError(
                        "Argument 1 to module/2 must be a list, got ${args[1].prologTypeName}",
                        args[1].sourceInformation as SourceLocation
                    )
                )
            }
        }

        return ParseResult(
            ModuleDeclaration(name, exportSelection),
            ParseResultCertainty.MATCHED,
            reportings
        )
    }

    /**
     * Converts the AST of a `:- use_module/1` or `:- use_module/2` directive into a [ModuleImport].
     *
     * Formally, the given AST must succeed this goal:
     *
     *     compound_name_arguments(ImportAST, use_module, Args),
     *     [Ref|_] = Args,
     *     compound_name_arguments(Ref, Group, [RefName]),
     *     atom(Group),
     *     atom(RefName),
     *     (length(Args, 1); length(Args, 2),
     *         [_, Selection] = Args,
     *         (
     *             is_list(Selection),
     *             member(SelectionE, Selection),
     *             valid_clause_indicator(SelectionE)
     *         ) ; (
     *             except(Exclusions) = Selection,
     *             is_list(Exclusions),
     *             member(ExclusionE, Exclusions),
     *             valid_clause_indicator(ExclusionE)
     *         )
     *     )
     *
     * @param importAST The instance of `use_module/1` or `use_module/2`
     */
    protected open fun convertModuleImport(importAST: CompoundTerm) : ParseResult<ModuleImport> {
        val args = importAST.arguments
        require(args.size in 1..2)

        val moduleRefTerm = args[0]
        if (moduleRefTerm !is CompoundTerm) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(
                SemanticError("Argument 1 to use_module/${args.size} must be a compound term, got ${moduleRefTerm.prologTypeName}", importAST.sourceInformation as SourceLocation)
            ))
        }

        if (moduleRefTerm.arity != 1 || moduleRefTerm.arguments[0] !is Atom) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(
                SemanticError("Illegal module reference: must be of arity 1 and the sole argument must be an atom", importAST.sourceInformation as SourceLocation)
            ))
        }

        val moduleReference = ModuleReference(moduleRefTerm.functor, (moduleRefTerm.arguments[0] as Atom).name)

        if (args.size == 1) {
            return ParseResult.of(FullModuleImport(moduleReference))
        }

        val selectionTerm = args[1]

        if (selectionTerm is PrologList) {
            val imports = mutableMapOf<ClauseIndicator, String>()
            val reportings = mutableSetOf<Reporting>()
            importTerms@for (importTerm in selectionTerm.elements) {
                if (importTerm !is CompoundTerm) {
                    reportings.add(SyntaxError(
                        "References to single predicates in argument 1 to use_module/2 must be compounds, got ${importTerm.prologTypeName}",
                        importTerm.sourceInformation as SourceLocation
                    ))
                    continue@importTerms
                }

                when (importTerm.functor) {
                    "/"  -> {
                        val indicatorResult = convertIdiomaticClauseIndicator(importTerm)
                        reportings.addAll(indicatorResult.reportings)
                        indicatorResult.item?.let { imports[it] = it.functor }
                    }
                    "as" -> {
                        val indicatorResult = convertIdiomaticClauseIndicator(importTerm.arguments[0])
                        reportings.addAll(indicatorResult.reportings)
                        if (indicatorResult.item == null) {
                            continue@importTerms
                        }
                        val aliasTerm = importTerm.arguments[1]
                        if (aliasTerm is Atom) {
                            imports[indicatorResult.item] = aliasTerm.name
                        } else {
                            reportings.add(SyntaxError(
                                "Predicate aliases in argument 1 to use_module/2 must be atoms, got ${aliasTerm.prologTypeName}",
                                aliasTerm.sourceInformation as SourceLocation
                            ))
                        }
                    }
                    else -> {
                        reportings.add(SyntaxError(
                            "References to single predicates in argument 1 to use_module/2 must unify with either _/_ or _/_ as _",
                            importTerm.sourceInformation as SourceLocation
                        ))
                    }
                }
            }

            return ParseResult(
                SelectiveModuleImport(moduleReference, imports),
                ParseResultCertainty.MATCHED,
                reportings
            )
        } else if (selectionTerm is CompoundTerm && selectionTerm.functor == "except" && selectionTerm.arity == 1) {
            val listTerm = selectionTerm.arguments[0]
            if (listTerm !is PrologList) {
                return ParseResult(null, ParseResultCertainty.MATCHED, setOf(SyntaxError(
                    "Argument 1 to except/1 in argument 1 to use_module/2 must be a list, got ${listTerm.prologTypeName}",
                    listTerm.sourceInformation as SourceLocation
                )))
            }

            val reportings = mutableSetOf<Reporting>()
            val except = mutableSetOf<ClauseIndicator>()

            for (exceptTerm in listTerm.elements) {
                val indicatorResult = convertIdiomaticClauseIndicator(exceptTerm)
                reportings.addAll(indicatorResult.reportings)
                indicatorResult.item?.let { except.add(it) }
            }

            return ParseResult(
                ExceptModuleImport(moduleReference, except),
                ParseResultCertainty.MATCHED,
                reportings
            )
        } else {
            throw PrologRuntimeException("argument 1 to use_module/2 must be either a list or an instance of except/1, got ${selectionTerm.prologTypeName}")
        }
    }

    /**
     * Converts a term that should denote a clause indicator to a [ClauseIndicator].
     *
     * Formally, the given AST must succeed this goal:
     *
     *     Term = /(Name, Arity),
     *     atom(Name),
     *     integer(Arity),
     *     Arity >= 0.
     * @param term the indicator AST
     */
    private fun convertIdiomaticClauseIndicator(term: Term): ParseResult<ClauseIndicator> {
        if (term !is CompoundTerm || term.arity != 2 || term.functor != "/") {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(SyntaxError(
                "Predicate indicators must be instances of `/`/2",
                term.sourceInformation as SourceLocation
            )))
        }

        val functorTerm = term.arguments[0]
        val arityTerm = term.arguments[1]

        if (functorTerm !is Atom) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(SyntaxError(
                "Predicate functors must be atoms, got ${functorTerm.prologTypeName}",
                functorTerm.sourceInformation as SourceLocation
            )))
        }

        if (arityTerm !is PrologInteger) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(SyntaxError(
                "Predicate arities must be integers, got ${functorTerm.prologTypeName}",
                arityTerm.sourceInformation as SourceLocation
            )))
        }

        if (arityTerm.toInteger() < 0) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(SyntaxError(
                "Predicate arity cannot be negative",
                arityTerm.sourceInformation as SourceLocation
            )))
        }

        if (arityTerm.toInteger() > Int.MAX_VALUE) {
            return ParseResult(null, ParseResultCertainty.MATCHED, setOf(SyntaxError(
                "Predicate arity cannot be negative",
                arityTerm.sourceInformation as SourceLocation
            )))
        }

        return ParseResult.of(ClauseIndicator.of(functorTerm.name, arityTerm.toInteger().toInt()))
    }

    companion object {
        protected fun <I : Term, R : Any> convertAndVisit(input: I, converter: (I) -> ParseResult<R>, visitor: (R, SourceLocation) -> Collection<Reporting>): Collection<Reporting> {
            val result = converter(input)
            if (result.item == null) {
                return result.reportings
            }

            return result.reportings + visitor(result.item, input.sourceInformation as SourceLocation)
        }
    }
}