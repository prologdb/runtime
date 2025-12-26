package com.github.prologdb.runtime.cliinterp

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.terminal
import com.github.ajalt.clikt.parameters.arguments.ProcessedArgument
import com.github.ajalt.clikt.parameters.arguments.RawArgument
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.arguments.convert
import com.github.ajalt.clikt.parameters.arguments.multiple
import com.github.ajalt.clikt.parameters.arguments.optional
import com.github.ajalt.clikt.parameters.options.NullableOption
import com.github.ajalt.clikt.parameters.options.OptionWithValues
import com.github.ajalt.clikt.parameters.options.RawOption
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.help
import com.github.ajalt.clikt.parameters.options.multiple
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.splitPair
import com.github.ajalt.mordant.input.KeyboardEvent
import com.github.ajalt.mordant.input.MouseTracking
import com.github.ajalt.mordant.input.enterRawMode
import com.github.ajalt.mordant.input.isCtrlC
import com.github.ajalt.mordant.terminal.Terminal
import com.github.ajalt.mordant.terminal.prompt
import com.github.prologdb.async.LazySequence
import com.github.prologdb.parser.ParseException
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.lexer.LineEndingNormalizer
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.StopCondition
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment
import com.github.prologdb.runtime.FullyQualifiedClauseIndicator
import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.module.CascadingModuleLoader
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleDeclaration
import com.github.prologdb.runtime.module.ModuleImport
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleNotFoundException
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.module.PredefinedModuleLoader
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.Atom
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.util.OperatorRegistry
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

private const val NEL = "\u0085"
private const val HISTORY_MAX_SIZE = 200


internal class ToplevelCommand(
    private val invokedInDir: Path,
) : CliktCommand(
    help = """
        Provides a REPL or executes prolog files from the filesystem.
        
        By default, will consult $(pwd)/main.pl and execute main:main/1, but this can be configured.
    """.trimIndent()
) {
    private val givenLibraryPaths: Map<String, Path> by option(metavar = "alias=path", names = arrayOf("--library", "-p"))
        .help("""
            register a library search path, e.g.$NEL
              given -p=foo=/my-pl-sources,$NEL
              when :- use_module(foo(test)) is executed$NEL
              then /my-pl-sources/test.pl will be consulted.$NEL
            An entry for app=<working directory> will always be present, unless overridden by an explicit entry for app=.
        """.trimIndent())
        .libraryPath()
        .multiple(default = listOf(LibraryPath(APP_LIBRARY_PATH_ALIAS, invokedInDir)), required = false)
        .associateAllUnique(keyName = "alias") { it.name to it.path }

    private val noReplFlag by option("--no-repl", help = "Just run the entrypoint goal, don't enter the REPL")
        .flag(default = false)

    private val entrypoint by argument("entrypoint", "Predicate to execute [before entering the REPL]. Must have arity 1, the single argument will be a list of the input arguments on the command line.")
        .fullyQualifiedPredicateIndicator()
        .optional()

    private val appArguments by argument("args", "Arguments for the prolog program, will be passed to the entry point.").multiple()

    override fun run() {
        val toplevelModuleLoader = PredefinedModuleLoader()
        val libraryPaths = HashMap(givenLibraryPaths)
        libraryPaths.putIfAbsent(APP_LIBRARY_PATH_ALIAS, invokedInDir)
        lateinit var runtime: PrologRuntimeEnvironment
        val moduleLoader = CascadingModuleLoader(
            toplevelModuleLoader,
            FilesystemModuleLoader(libraryPaths, parser) { DefaultModuleSourceFileVisitor(runtime, DefaultModuleSourceFileVisitor.DEFAULT_IMPORTS + ADDITIONAL_DEFAULT_IMPORTS) },
            ModuleLoader.discoverOnClasspath(),
        )

        runtime = DefaultPrologRuntimeEnvironment(moduleLoader)
        entrypoint?.let { entrypoint ->
            assureEntrypointModuleLoaded(runtime, libraryPaths, entrypoint)
            handlePrologError {
                val solutions = runtime.fulfill(entrypoint.moduleName, PredicateInvocationQuery(CompoundTerm(
                    entrypoint.indicator.functor,
                    arrayOf(PrologList(appArguments.map { PrologString(it) })),
                )))
                solutions.consumeAll().get()
            }
        }

        if (noReplFlag) {
            return
        }

        repl(runtime, setUpToplevelModule(runtime, toplevelModuleLoader))
    }

    private fun setUpToplevelModule(runtime: PrologRuntimeEnvironment, loader: PredefinedModuleLoader): Module {
        val visitor = DefaultModuleSourceFileVisitor(
            runtime,
            DefaultModuleSourceFileVisitor.DEFAULT_IMPORTS + ADDITIONAL_DEFAULT_IMPORTS,
        )
        visitor.visitModuleDeclaration(ModuleDeclaration("user"), SourceLocation(
            SourceUnit("repl"), 1, 1, 0
        ))

        val moduleResult = visitor.buildResult()
        check(moduleResult.isSuccess) { moduleResult.reportings.joinToString("\n") }
        val module = moduleResult.item!!

        loader.registerModule("toplevel", module)

        return module
    }

    private fun repl(runtime: DefaultPrologRuntimeEnvironment, toplevelModule: Module) {
        runtime.assureModuleLoaded(ModuleReference("toplevel", toplevelModule.declaration.moduleName))
        val history = ArrayList<String>()

        read@while (true) {
            val query = promptQuery(toplevelModule.localOperators, history)
            while (history.size > HISTORY_MAX_SIZE) {
                history.removeFirst()
            }

            val solutions = runtime.fulfill(toplevelModule.declaration.moduleName, query)
            evalAndPrint@while (true) {
                val solution = handlePrologError {
                    solutions.tryAdvance()
                }

                if (solution == null) {
                    echo("false")
                    break@evalAndPrint
                }

                val solutionStr = if (solution.isEmpty) "true" else {
                    solution.entries.joinToString(
                        separator = " ,\n",
                        transform = { "${it.first} = ${it.second.toStringUsingOperatorNotations(toplevelModule.localOperators)}" }
                    )
                }

                if (solutions.state == LazySequence.State.DEPLETED) {
                    echo(solutionStr)
                    echo(".")
                    break@evalAndPrint
                }

                val action = terminal.prompt(solutionStr, promptSuffix = " ", choices = setOf(
                    ";", "."
                ))

                when (action) {
                    ";" -> {
                        continue@evalAndPrint
                    }
                    "." -> {
                        break@evalAndPrint
                    }
                    else -> {
                        error("Unknown Action (; .)")
                    }
                }
            }

            solutions.close()
        }
    }

    private fun promptQuery(operators: OperatorRegistry, inputHistory: MutableList<String>): Query {
        while (true) {
            val query = terminal.promptWithHistory(inputHistory)
            val queryResult = parser.parseQuery(lex(SourceUnit("user input"), query), operators)
            if (queryResult.isSuccess) {
                return queryResult.item!!
            }

            queryResult.reportings.forEach {
                echo("[${it.level.name}] ${it.message}")
            }
        }
    }

    private fun assureEntrypointModuleLoaded(
        runtime: PrologRuntimeEnvironment,
        libraryPaths: Map<String, Path>,
        entrypoint: FullyQualifiedClauseIndicator,
    ) {
        val loadingExByPathAlias = mutableMapOf<String, ModuleNotFoundException>()
        for ((alias, _)  in libraryPaths) {
            try {
                runtime.assureModuleLoaded(ModuleReference(alias, entrypoint.moduleName))
                return
            }
            catch (ex: ModuleNotFoundException) {
                loadingExByPathAlias[alias] = ex
            }
        }

        if (noReplFlag || entrypoint != DEFAULT_ENTRYPOINT) {
            echo("Couldn't load module ${entrypoint.moduleName} from any of the provided library paths:")
            for ((alias, ex) in loadingExByPathAlias) {
                echo("  $alias: ${ex.message}")
            }
            exitProcess(1)
        } else {
            // entering REPL, maybe no need to run an entrypoint
        }
    }

    private inline fun <T> handlePrologError(crossinline action: () -> T): T {
        try {
            return action()
        }
        catch (ex: ModuleNotFoundException) {
            echo("Module ${ex.reference} not found" + (ex.message?.let { " ($it)" } ?: ""), err = true)
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

/**
 * Like [Terminal.prompt] plus the following features:
 * * the user can navigate the [history] using ArrowUp + ArrowDown
 * * if the user modifies a history entry before confirming, it gets added to the history
 * * if the user enters a novel input not from history, it gets added to the history
 *
 * @param addToHistory is given novel inputs or modified history entries. If this function returns false, the new/modified
 *                     input will silently _not_ be added to [history]. Use to filter e.g. blank inputs.
 */
fun Terminal.promptWithHistory(
    history: MutableList<String>,
    addToHistory: (String) -> Boolean = String::isNotBlank,
): String {
    enterRawMode(mouseTracking = MouseTracking.Off).use { rawMode ->
        var nextInput = ""
        var historyIndex = history.size
        var currentInput = StringBuilder(nextInput)
        var currentInputModified = false
        var cursorIndex = 0
        cursor.show()

        while (true) {
            val event = rawMode.readKey()
            when {
                event.isEnter -> {
                    rawPrint("\n")
                    break
                }
                event.isCtrlC -> throw InputAbortedException()
                event.isArrowLeft -> {
                    if (cursorIndex == 0) {
                        continue
                    }
                    cursorIndex--
                    cursor.move {
                        left(1)
                    }
                }
                event.isArrowRight -> {
                    if (cursorIndex >= currentInput.length - 1) {
                        continue
                    }
                    cursorIndex++
                    cursor.move {
                        right(1)
                    }
                }
                event.isHome -> {
                    cursor.move {
                        left(cursorIndex)
                    }
                    cursorIndex = 0
                }
                event.isEnd -> {
                    cursor.move {
                        right(currentInput.length - cursorIndex)
                        cursorIndex = currentInput.length
                    }
                }
                event.isBackspace -> {
                    if (cursorIndex == 0) {
                        continue
                    }

                    currentInput.deleteCharAt(cursorIndex - 1)
                    currentInputModified = true
                    cursorIndex--
                    cursor.move {
                        left(1)
                    }
                    rawPrint(" ")
                    cursor.move {
                        left(1)
                    }
                }
                event.isDelete -> {
                    if (cursorIndex >= currentInput.length) {
                        continue
                    }
                    currentInput.deleteCharAt(cursorIndex)
                    currentInputModified = true

                    val tailLength = currentInput.length - cursorIndex
                    rawPrint(currentInput.substring(cursorIndex, currentInput.length))
                    rawPrint(" ")
                    cursor.move {
                        left(tailLength + 1)
                    }
                }
                event.isArrowUp -> {
                    if (historyIndex <= 0) {
                        continue
                    }

                    // clear line
                    cursor.move {
                        left(cursorIndex)
                    }
                    rawPrint(" ".repeat(currentInput.length))
                    cursor.move {
                        left(currentInput.length)
                    }

                    if (historyIndex == history.size) {
                        // so it can be restored if the user navigates back to after the history
                        nextInput = currentInput.toString()
                    }

                    historyIndex--
                    currentInput = StringBuilder(history[historyIndex])
                    currentInputModified = false
                    rawPrint(currentInput.toString())
                    cursorIndex = currentInput.length
                }
                event.isArrowDown -> {
                    if (historyIndex >= history.size) {
                        continue
                    }

                    cursor.move {
                        left(cursorIndex)
                    }
                    rawPrint(" ".repeat(currentInput.length))
                    cursor.move {
                        left(currentInput.length)
                    }

                    historyIndex++
                    val nextCurrentInput = if (historyIndex < history.size) {
                        history[historyIndex]
                    } else {
                        nextInput
                    }

                    currentInput = StringBuilder(nextCurrentInput)
                    currentInputModified = false
                    rawPrint(currentInput.toString())
                    cursorIndex = currentInput.length
                }
                else -> {
                    currentInput.insert(cursorIndex, event.key)
                    currentInputModified = true
                    cursorIndex += event.key.length
                    rawPrint(event.key)
                    if (cursorIndex < currentInput.length) {
                        val tailLength = currentInput.length - cursorIndex
                        rawPrint(currentInput.substring(cursorIndex, currentInput.length))
                        cursor.move {
                            left(tailLength)
                        }
                    }
                }
            }
        }

        val resultAsString = currentInput.toString()
        if ((historyIndex == history.size || currentInputModified) && addToHistory(resultAsString)) {
            history.add(resultAsString)
        }

        return resultAsString
    }
}

class InputAbortedException : RuntimeException()

val KeyboardEvent.isEnter: Boolean get()= key == "Enter" && !ctrl && !alt && !shift;
val KeyboardEvent.isBackspace: Boolean get()= key == "Backspace" && !ctrl && !alt && !shift;
val KeyboardEvent.isArrowLeft: Boolean get()= key == "ArrowLeft" && !ctrl && !alt && !shift;
val KeyboardEvent.isArrowRight: Boolean get()= key == "ArrowRight" && !ctrl && !alt && !shift;
val KeyboardEvent.isArrowUp: Boolean get()= key == "ArrowUp" && !ctrl && !alt && !shift;
val KeyboardEvent.isArrowDown: Boolean get()= key == "ArrowDown" && !ctrl && !alt && !shift;
val KeyboardEvent.isDelete: Boolean get()= key == "Delete" && !ctrl && !alt && !shift;
val KeyboardEvent.isHome: Boolean get()= key == "Home" && !ctrl && !alt && !shift;
val KeyboardEvent.isEnd: Boolean get()= key == "End" && !ctrl && !alt && !shift;