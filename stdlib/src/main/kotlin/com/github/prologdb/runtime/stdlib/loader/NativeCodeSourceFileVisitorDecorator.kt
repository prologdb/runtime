package com.github.prologdb.runtime.stdlib.loader

import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SemanticError
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.module.ModuleDeclaration
import com.github.prologdb.runtime.stdlib.NativeCodeRule
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.util.DefaultOperatorRegistry
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorRegistry
import com.github.prologdb.runtime.util.OperatorType

/**
 * Implements the directive `native/1`: it takes a clause indicator
 * as the sole argument. When a `native/1` directive is seen, the native
 * implementation is looked up from [availableNativeCode] and passed to
 * the [delegate]s [SourceFileVisitor.visitClause] method.
 *
 * Also defines a module-local operator `op(1150, fx, native)` so that `native/1`
 * directives can be written as e.g.
 *
 * ```prolog
 * :- native call/1.
 * :- native findall/3.
 * ```
 */
class NativeCodeSourceFileVisitorDecorator<Result : Any>(
    private val delegate: SourceFileVisitor<Result>,
    private val availableNativeCode: Map<ClauseIndicator, NativeCodeRule>,
    private val parser: PrologParser = PrologParser()
) : SourceFileVisitor<Result> by delegate {
    private val clausesWithPrologSource = mutableSetOf<ClauseIndicator>()
    private val declaredNative = mutableSetOf<ClauseIndicator>()

    override fun visitModuleDeclaration(declaration: ModuleDeclaration, location: SourceLocation) {
        delegate.visitModuleDeclaration(declaration, location)

        delegate.visitDirective(CompoundTerm("op", arrayOf(PrologInteger(1150), Atom("fx"), Atom("native"))))
    }

    override fun visitDirective(command: CompoundTerm): Collection<Reporting> {
        if (command.functor == "native" && command.arity == 1) {
            val clauseIndicatorResult = parser.parseIdiomaticClauseIndicator(command.arguments[0])
            val reportings = clauseIndicatorResult.item
                ?.let { this.visitNativeImport(it, command.sourceInformation as SourceLocation) }
                ?: emptySet()

            return reportings + clauseIndicatorResult.reportings
        }

        return delegate.visitDirective(command)
    }

    private fun visitNativeImport(indicator: ClauseIndicator, location: SourceLocation): Collection<Reporting> {
        if (indicator in clausesWithPrologSource) {
            return setOf(SemanticError(
                "Predicate $indicator has prolog-source clauses, cannot also be native.",
                location
            ))
        }

        val nativeCode = availableNativeCode[indicator]
            ?: return setOf(SemanticError(
                "Did not find a native implementation for $indicator",
                location
            ))

        if (declaredNative.add(indicator)) {
            delegate.visitClause(nativeCode, location)
            return emptySet()
        } else {
            return setOf(SemanticError("$indicator declared native twice", location))
        }
    }

    override fun visitClause(clause: Clause, location: SourceLocation): Collection<Reporting> {
        val indicator = ClauseIndicator.Companion.of(clause)
        if (indicator in declaredNative) {
            return setOf(SemanticError(
                "Predicate $indicator declared as native, cannot also have prolog-source clauses.",
                location
            ))
        }

        clausesWithPrologSource.add(indicator)
        return delegate.visitClause(clause, location)
    }
}
