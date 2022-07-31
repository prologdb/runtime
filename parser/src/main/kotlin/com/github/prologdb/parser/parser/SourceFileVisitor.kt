package com.github.prologdb.parser.parser

import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.module.ModuleDeclaration
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.util.OperatorRegistry

/**
 * To be used with [PrologParser.parseSourceFile].
 */
interface SourceFileVisitor<out Result : Any> {
    /**
     * Contrary to the other methods, this one should not modify the state of the visitor. It's merely here
     * so that the visitor can (re)define the syntax for the module declaration if necessary.
     * @param firstTermOfFile literally that, including the `':-'/1` wrapper for directives
     * @return if [firstTermOfFile] is not at all a module declaration: null. If it is a module declaration:
     * a parse result reflecting the parse result, including any (fatal) errors.
     */
    fun tryParseModuleDeclaration(firstTermOfFile: Term): ParseResult<ModuleDeclaration>?

    /**
     * Is invoked **during the parsing stage** with the effective declaration (either the one parsed
     * or the implicit one given to [PrologParser.parseSourceFile]).
     */
    fun visitModuleDeclaration(declaration: ModuleDeclaration, location: SourceLocation)

    /**
     * The operators available while parsing. New operator definitions passed to [visitDirective] must be
     * made available here so that they can take effect for the rest of the file.
     */
    val operators: OperatorRegistry

    /**
     * Called when the parser encounters a top-level term that is an instance of `:-/1`.
     */
    fun visitDirective(command: CompoundTerm): Collection<Reporting>

    /**
     * Called when the parser encounters a top-level term that is a fact ([CompoundTerm]) or a [Rule] (instance of `:-/2`).
     */
    fun visitClause(clause: Clause, location: SourceLocation): Collection<Reporting>

    /**
     * Called when the parser encounters a term on the top level that is not a clause.
     * The default behaviour is to report an error.
     */
    fun visitNonClause(item: Term): Collection<Reporting> {
        return listOf(SemanticError(
            "A ${item.prologTypeName} is not a top level declaration, expected a fact, a rule or a directive.",
            item.sourceInformation as SourceLocation
        ))
    }

    /**
     * Is invoked once when EOF of the source file has been reached.
     * @return first: the result, null if the source was so low-quality that none could be constructed
     *         second: any more warnings+errors that have been found
     */
    fun buildResult(): ParseResult<Result>
}
