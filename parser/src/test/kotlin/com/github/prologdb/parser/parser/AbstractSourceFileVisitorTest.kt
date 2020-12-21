package com.github.prologdb.parser.parser

import com.github.prologdb.parser.ModuleDeclaration
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceLocationRange
import com.github.prologdb.runtime.Clause
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologDictionary
import com.github.prologdb.runtime.term.PrologInteger
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType
import io.kotlintest.forOne
import io.kotlintest.matchers.beEmpty
import io.kotlintest.matchers.should
import io.kotlintest.matchers.shouldBe
import io.kotlintest.matchers.shouldNotBe
import io.kotlintest.specs.FreeSpec

class AbstractSourceFileVisitorTest : FreeSpec() { init {
    val visitor = TestingVisitor()
    "invalid :-op/3" - {
        "non-numeric precedence" {
            val declaration = CompoundTerm("op", arrayOf(Atom("invalid"), Atom("xfx"), Atom("opname"))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            val reporting = reportings.first()
            reporting.message shouldBe "operator precedence must be an integer"
            visitor.callsToVisitOperatorDefinition should beEmpty()
        }

        "precedence out of range below" {
            val declaration = CompoundTerm("op", arrayOf(PrologInteger(-10), Atom("xfx"), Atom("opname"))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            reportings.first().message shouldBe "operator precedence must be between 0 and 1200 (inclusive)"
            visitor.callsToVisitOperatorDefinition.size shouldBe 1
            visitor.callsToVisitOperatorDefinition.first().first.precedence shouldBe 0.toShort()
        }

        "precedence out of range above" {
            val declaration = CompoundTerm("op", arrayOf(PrologInteger(1201), Atom("xfx"), Atom("opname"))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            val reporting = reportings.first()
            reporting.message shouldBe "operator precedence must be between 0 and 1200 (inclusive)"
            visitor.callsToVisitOperatorDefinition.size shouldBe 1
            visitor.callsToVisitOperatorDefinition.first().first.precedence shouldBe 1200.toShort()
        }

        "type argument not atom" {
            val declaration = CompoundTerm("op", arrayOf(PrologInteger(400), PrologList(emptyList()), Atom("opname"))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            val reporting = reportings.first()
            reporting.message shouldBe "operator type: expected atom but got list"
            visitor.callsToVisitOperatorDefinition should beEmpty()
        }

        "undefined type" {
            val declaration = CompoundTerm("op", arrayOf(PrologInteger(400), Atom("yfy"), Atom("opname"))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            val reporting = reportings.first()
            reporting.message shouldBe "yfy is not a known operator type"
            visitor.callsToVisitOperatorDefinition should beEmpty()
        }

        "name argument not atom" {
            val declaration = CompoundTerm("op", arrayOf(PrologInteger(400), Atom("xfx"), PrologList(emptyList()))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            val reporting = reportings.first()
            reporting.message shouldBe "operator name: expected atom but got list"
            visitor.callsToVisitOperatorDefinition should beEmpty()
        }
    }
    "valid :-op/3" {
        val declaration = CompoundTerm("op", arrayOf(PrologInteger(800), Atom("xfx"), Atom("myop"))).withMockSourceLocation()
        val reportings = visitor.visitDirective(declaration)
        reportings should beEmpty()
        visitor.callsToVisitOperatorDefinition.size shouldBe 1
        visitor.callsToVisitOperatorDefinition.first().first.precedence shouldBe 800.toShort()
        visitor.callsToVisitOperatorDefinition.first().first.type shouldBe OperatorType.XFX
        visitor.callsToVisitOperatorDefinition.first().first.name shouldBe "myop"
    }
    "invalid :-module/1" {
        val declaration = CompoundTerm("module", arrayOf(PrologList(emptyList()))).withMockSourceLocation()
        val reportings = visitor.visitDirective(declaration)
        reportings.size shouldBe 1
        reportings.first().message shouldBe "Argument 0 to module/1 must be an atom, got list"
        visitor.callsToVisitModuleDeclaration should beEmpty()
    }
    "valid :-module/1" {
        val declaration = CompoundTerm("module", arrayOf(Atom("mymodule"))).withMockSourceLocation()
        val reportings = visitor.visitDirective(declaration)
        reportings should beEmpty()
        visitor.callsToVisitModuleDeclaration.size shouldBe 1
        visitor.callsToVisitModuleDeclaration.first().first.moduleName shouldBe "mymodule"
        visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates shouldBe null
    }
    "invalid :-module/2" - {
        "second argument not a list" {
            val declaration = CompoundTerm("module", arrayOf(Atom("name"), Atom("bla"))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            reportings.first().message shouldBe "Argument 1 to module/2 must be a list, got atom"
            visitor.callsToVisitModuleDeclaration.size shouldBe 1
            visitor.callsToVisitModuleDeclaration.first().first.moduleName shouldBe "name"
            visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates shouldBe null
        }
        "export list with invalid clause indicator" {
            val declaration = CompoundTerm("module", arrayOf(Atom("name"), PrologList(listOf(Atom("predname"))))).withMockSourceLocation()
            val reportings = visitor.visitDirective(declaration)
            reportings.size shouldBe 1
            reportings.first().message shouldBe "Predicate indicators must be instances of `/`/2"
            visitor.callsToVisitModuleDeclaration.size shouldBe 1
            visitor.callsToVisitModuleDeclaration.first().first.moduleName shouldBe "name"
            visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates shouldNotBe null
            visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates!! should beEmpty()
        }
    }
    "valid :-module/2" {
        val exports = listOf(
            CompoundTerm("/", arrayOf(Atom("predname"), PrologInteger(2))),
            CompoundTerm("/", arrayOf(Atom("otherpred"), PrologInteger(4)))
        )
        val declaration = CompoundTerm("module", arrayOf(Atom("name"), PrologList(exports))).withMockSourceLocation()
        val reportings = visitor.visitDirective(declaration)
        reportings should beEmpty()
        visitor.callsToVisitModuleDeclaration.size shouldBe 1
        visitor.callsToVisitModuleDeclaration.first().first.moduleName shouldBe "name"
        visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates shouldNotBe null
        visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates!!.size shouldBe 2

        forOne(visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates!!) {
            it.functor shouldBe "predname"
            it.arity shouldBe 2
        }

        forOne(visitor.callsToVisitModuleDeclaration.first().first.exportedPredicates!!) {
            it.functor shouldBe "otherpred"
            it.arity shouldBe 4
        }
    }
} }

private fun <T : Term> T.withMockSourceLocation(): T {
    val nestedMapped : T = when(this) {
        is CompoundTerm -> CompoundTerm(functor, arguments.map { it.withMockSourceLocation() }.toTypedArray()) as T
        is PrologList -> PrologList(elements.map { it.withMockSourceLocation() }, tail) as T
        is PrologDictionary -> PrologDictionary(pairs.map { (key, value) ->
            Pair(key.withMockSourceLocation(), value.withMockSourceLocation())
        }.toMap(), tail) as T
        else -> this
    }

    nestedMapped.sourceInformation = SourceLocationRange(
        SourceLocation.EOF,
        SourceLocation.EOF
    )
    return nestedMapped
}

private class TestingVisitor : AbstractSourceFileVisitor<Unit>() {
    val callsToVisitOperatorDefinition = mutableListOf<Pair<OperatorDefinition, SourceLocation>>()
    val callsToVisitDynamicDeclaration = mutableListOf<Pair<ClauseIndicator, SourceLocation>>()
    val callsToVisitModuleDeclaration = mutableListOf<Pair<ModuleDeclaration, SourceLocation>>()
    val callsToVisitImport = mutableListOf<Pair<ModuleImport, SourceLocation>>()
    val callsToVisitClause = mutableListOf<Pair<Clause, SourceLocation>>()

    override fun visitOperatorDefinition(
        definition: OperatorDefinition,
        location: SourceLocation
    ): Collection<Reporting> {
        callsToVisitOperatorDefinition.add(Pair(definition, location))
        return super.visitOperatorDefinition(definition, location)
    }

    override fun visitDynamicDeclaration(
        clauseIndicator: ClauseIndicator,
        location: SourceLocation
    ): Collection<Reporting> {
        callsToVisitDynamicDeclaration.add(Pair(clauseIndicator, location))
        return emptyList()
    }

    override fun visitModuleDeclaration(
        declaration: ModuleDeclaration,
        location: SourceLocation
    ): Collection<Reporting> {
        callsToVisitModuleDeclaration.add(Pair(declaration, location))
        return emptyList()
    }

    override fun visitImport(import: ModuleImport, location: SourceLocation): Collection<Reporting> {
        callsToVisitImport.add(Pair(import, location))
        return emptyList()
    }

    override fun visitClause(clause: Clause, location: SourceLocation): Collection<Reporting> {
        callsToVisitClause.add(Pair(clause, location))
        return emptyList()
    }

    override fun buildResult(): ParseResult<Unit> {
        return ParseResult.of(Unit)
    }
}