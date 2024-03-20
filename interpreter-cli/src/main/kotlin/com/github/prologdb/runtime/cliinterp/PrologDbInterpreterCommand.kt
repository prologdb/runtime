package com.github.prologdb.runtime.cliinterp

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.arguments.ProcessedArgument
import com.github.ajalt.clikt.parameters.arguments.RawArgument
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.arguments.convert
import com.github.ajalt.clikt.parameters.arguments.multiple
import com.github.ajalt.clikt.parameters.arguments.transformAll
import com.github.ajalt.clikt.parameters.options.NullableOption
import com.github.ajalt.clikt.parameters.options.OptionWithValues
import com.github.ajalt.clikt.parameters.options.RawOption
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.help
import com.github.ajalt.clikt.parameters.options.multiple
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.splitPair
import com.github.prologdb.parser.ParseException
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.StopCondition
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.module.CascadingModuleLoader
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import java.nio.file.Path
import java.nio.file.Paths
import kotlin.io.path.exists
import kotlin.io.path.isDirectory
import kotlin.system.exitProcess

private val parser = PrologParser()
private fun lex(unit: SourceUnit, code: String): Lexer {
    return Lexer(unit, LineEndingNormalizer(code.iterator()))
}

private const val APP_LIBRARY_PATH_ALIAS = "app"
private val DEFAULT_ENTRYPOINT = FullyQualifiedClauseIndicator("main", ClauseIndicator.of("main", 1))
private val ADDITIONAL_DEFAULT_IMPORTS: Set<ModuleImport.Full> = listOf(
    ModuleReference("script", "io")
).map(ModuleImport::Full).toSet()

internal class PrologDbInterpreterCommand(
    private val invokedInDir: Path,
) : CliktCommand() {
    private val givenLibraryPaths by option(metavar = "alias=path", names = arrayOf("--library", "-p"))
        .help("""
            register a library search path, e.g. library=/my-pl-sources to look for sources in /my-pl-sources/test.pl when library(test) is imported.
            An entry for app=<working directory> will always be present
        """.trimIndent())
        .libraryPath()
        .multiple(default = listOf(LibraryPath(APP_LIBRARY_PATH_ALIAS, invokedInDir)), required = false)
        .associateAllUnique(keyName = "alias") { it.name to it.path }

    private val entrypoint by argument("entrypoint", "The predicate to call. Must have arity 1, for arity 1 will be given a list of the input arguments on the command line.")
        .fullyQualifiedPredicateIndicator()
        .transformAll(1, false, DEFAULT_ENTRYPOINT.toString()) { it.firstOrNull() ?: DEFAULT_ENTRYPOINT }

    private val appArguments by argument("args", "Arguments for the prolog program, will be passed to the entry point.").multiple()

    override fun run() {
        val libraryPaths = HashMap(givenLibraryPaths)
        libraryPaths.putIfAbsent(APP_LIBRARY_PATH_ALIAS, invokedInDir)
        lateinit var runtime: PrologRuntimeEnvironment
        val moduleLoader = CascadingModuleLoader(
            FilesystemModuleLoader(libraryPaths, parser) { DefaultModuleSourceFileVisitor(runtime, DefaultModuleSourceFileVisitor.DEFAULT_IMPORTS + ADDITIONAL_DEFAULT_IMPORTS) },
            ModuleLoader.discoverOnClasspath(),
        )

        runtime = DefaultPrologRuntimeEnvironment(moduleLoader)
        try {
            runtime.assureModuleLoaded(ModuleReference(APP_LIBRARY_PATH_ALIAS, DEFAULT_ENTRYPOINT.moduleName))
            val solutions = runtime.fulfill(entrypoint.moduleName, PredicateInvocationQuery(CompoundTerm(
                entrypoint.indicator.functor,
                arrayOf(PrologList(appArguments.map { PrologString(it) })),
            )))
            solutions.consumeAll().get()
        }
        catch (ex: ModuleNotFoundException) {
            echo("Module ${ex.reference} not found" + ex.message?.let { " ($it)" } ?: "", err = true)
            exitProcess(1)
        }
        catch (ex: ParseException) {
            echo(ex.reporting.toString(), err = true)
            exitProcess(1)
        }
        catch (ex: PrologException) {
            echo(ex.formattedPrologStackTrace, err = true)
            exitProcess(2)
        }
    }
}

private data class LibraryPath(val name: String, val path: Path) {
    override fun toString() = "$name=$path"
}

private fun RawOption.libraryPath(): NullableOption<LibraryPath, LibraryPath> {
    return splitPair("=").convert("alias=path") { (name, pathStr) ->
        val path = Paths.get(pathStr)
        if (path.exists() && !path.isDirectory()) {
            fail("library paths must point to directories")
        }

        LibraryPath(name, path)
    }
}

private fun RawArgument.fullyQualifiedPredicateIndicator(): ProcessedArgument<FullyQualifiedClauseIndicator, FullyQualifiedClauseIndicator> {
    return convert { rawIndicator ->
        val fqiCiTermResult = parser.parseTerm(
            lex(SourceUnit("CLI option ${this.name}"), rawIndicator),
            ISOOpsOperatorRegistry,
            StopCondition.STOP_AT_EOF,
        )

        fqiCiTermResult.reportings.find { it.level >= Reporting.Level.ERROR }
            ?.let { fail("couldn't parse prolog term: $it") }

        val fqiCiTerm = fqiCiTermResult.item!!

        if (fqiCiTerm !is CompoundTerm || fqiCiTerm.functor != ":" || fqiCiTerm.arity != 2) {
            fail("the clause indicator must be an instance of :/2, they have the shape module:predicate/arity")
        }
        val moduleNameTerm = fqiCiTerm.arguments[0]
        if (moduleNameTerm !is Atom) {
            fail("the module name must be an atom, got a ${moduleNameTerm.prologTypeName}")
        }

        val simpleIndicatorResult = parser.parseIdiomaticClauseIndicator(fqiCiTerm.arguments[1])
        simpleIndicatorResult.reportings.find { it.level >= Reporting.Level.ERROR }
            ?.let { fail("couldn't parse the clause indicator: ${it.message}, must be of the shape module:predicate/arity") }

        val simpleCi = simpleIndicatorResult.item!!

        FullyQualifiedClauseIndicator(moduleNameTerm.name, simpleCi)
    }
}

private fun <ValueT, EachT, K, V> OptionWithValues<List<EachT>, EachT, ValueT>.associateAllUnique(
    collectionType: () -> MutableMap<K, V> = ::HashMap,
    keyName: String,
    toMapEntry: (EachT) -> Pair<K, V>,
): OptionWithValues<Map<K, V>, EachT, ValueT> {
    return copy(
        transformValue,
        transformEach,
        transformAll = {
            val all = transformAll(it)
            val map = collectionType()
            for (item in all) {
                val (key, value) = toMapEntry(item)
                if (map.putIfAbsent(key, value) != null) {
                    fail("$keyName must be unique, $key is duplicate")
                }
            }
            map
        },
        { },
    )
}