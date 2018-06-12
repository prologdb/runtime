import com.github.prologdb.parser.ParsedTerm
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.ReportingException
import com.github.prologdb.parser.SyntaxError
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.knowledge.DefaultKnowledgeBase
import com.github.prologdb.runtime.knowledge.library.*
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.AnonymousVariable
import com.github.prologdb.runtime.term.Predicate
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term
import com.github.tmarsteel.ktprolog.parser.ParseResult
import io.kotlintest.matchers.fail
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.specs.FreeSpec
import org.springframework.core.io.support.PathMatchingResourcePatternResolver
import java.lang.invoke.MethodHandles
import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Paths
import com.github.prologdb.runtime.term.List as PrologList


/** runs the tests found in the *.test.pl files in the prolog tests directory */
class PrologTest : FreeSpec() { init {
    for (prologTestFileURI in prologTestFiles) {
        val path = Paths.get(prologTestFileURI.path)
        "${path.fileName}" - {
            // run the test
            val callback = object : TestResultCallback {
                override fun onTestSuccess(testName: String) {
                    testName.invoke {
                        // implicit success
                        true shouldEqual true // for good measure
                    }
                }

                override fun onTestFailure(testName: String, message: String) {
                    testName.invoke {
                        fail(message)
                    }
                }

                override fun onTestError(testName: String, error: Throwable) {
                    testName.invoke {
                        throw error
                    }
                }

                override fun onTestParseError(errors: Collection<Reporting>) {
                    errors.forEach(::println)

                    throw RuntimeException("Failed to parse test case file, see STDOUT for errors.")
                }
            }

            runPrologTestFile(prologTestFileURI, callback)
        }
    }
}
    private fun runPrologTestFile(uri: URI, callback: TestResultCallback) {
        val parseResult = parseFile(uri)

        if (!parseResult.isSuccess || parseResult.reportings.isNotEmpty()) {
            callback.onTestParseError(parseResult.reportings)
            return
        }

        val library = parseResult.item ?: throw RuntimeException("Invalid return value from parser: is success but item is null")
        val testCases = getPrologTestCases(library, callback::onTestParseError)

        testCases.forEach { it.runWith(callback) }
    }

    private fun parseFile(uri: URI): ParseResult<Library> {
        val fileContent = uri.toURL().openStream().bufferedReader(Charset.forName("UTF-8")).use {
            it.readText()
        }
        val sourceUnit = SourceUnit(uri.path)
        val lexer = Lexer(fileContent.iterator(), SourceLocation(sourceUnit, 1, 0, 0))

        return PrologParser().parseLibrary(lexer, { createFreshTestingKnowledgeBase().library })
    }

    private fun getPrologTestCases(library: Library, parseErrorCallback: (Collection<Reporting>) -> Any?): Set<PrologTestCase> {
        val by2Results = library.findFor(Predicate("by", arrayOf(AnonymousVariable, AnonymousVariable)))
        val testCases = mutableSetOf<PrologTestCase>()
        for (by2instance in by2Results) {
            if (by2instance !is Predicate) continue
            val arg0 = by2instance.arguments[0]
            val arg1 = by2instance.arguments[1]
            if (arg0 !is Predicate) continue
            if (arg1 !is PrologList) continue

            if (arg0.name != "test") continue
            if (arg0.arity != 1) continue
            if (arg0.arguments[0] !is PrologString) continue

            val testName = (arg0.arguments[0] as PrologString).toKotlinString()
            val testQuery = try {
                goalListToAndQuery(arg1.elements)
            } catch (ex: ReportingException) {
                parseErrorCallback(setOf(ex.reporting))
                continue
            }

            testCases.add(object : PrologTestCase {
                override val name = testName

                override fun runWith(callback: TestResultCallback) {
                    val runtimeEnv = DefaultKnowledgeBase(library.clone())
                    val result = runtimeEnv.fulfill(testQuery)
                    val hadAtLeastOneSolution = result.tryAdvance() != null
                    result.close()

                    if (hadAtLeastOneSolution) {
                        callback.onTestSuccess(testName)
                    } else {
                        callback.onTestFailure(testName, "Query had no solutions: $testQuery")
                    }
                }
            })
        }

        return testCases
    }
}

private val prologTestFiles: List<URI>
    get() {
        val classLoader = MethodHandles.lookup().javaClass.classLoader
        val resolver = PathMatchingResourcePatternResolver(classLoader)

        return resolver.getResources("classpath:**/*.test.pl").map { it.uri }
    }

private interface TestResultCallback {
    fun onTestSuccess(testName: String)

    fun onTestFailure(testName: String, message: String)

    fun onTestError(testName: String, error: Throwable)

    fun onTestParseError(errors: Collection<Reporting>)
}

private interface PrologTestCase {
    val name: String
    fun runWith(callback: TestResultCallback)
}

private fun createFreshTestingKnowledgeBase(): DefaultKnowledgeBase {
    val kb = DefaultKnowledgeBase()
    kb.operatorRegistry.defineOperator(OperatorDefinition(100, OperatorType.FX, "test"))
    kb.operatorRegistry.defineOperator(OperatorDefinition(800, OperatorType.XFX, "by"))

    return kb
}

private fun Library.clone(): MutableLibrary {
    val entryStore = DoublyIndexedLibraryEntryStore()
    exports.forEach(entryStore::add)

    val opRegistry = DefaultOperatorRegistry(false)
    allOperators.forEach(opRegistry::defineOperator)

    return SimpleLibrary(entryStore, opRegistry)
}

private fun goalListToAndQuery(goals: Collection<Term>): Query {
    return AndQuery(
        goals.map { it.asPredicate() }
            .map(::predicateToQuery)
            .toTypedArray()
    )
}

private fun predicateToQuery(predicate: Predicate): Query {
    if (predicate.name == ",") {
        val goals = mutableListOf<Query>()
        goals.add(predicateToQuery(predicate.arguments[0].asPredicate()))

        var pivot = predicate.arguments[1].asPredicate()
        while (pivot.name == ",") {
            goals.add(predicateToQuery(pivot.arguments[0].asPredicate()))
            pivot = pivot.arguments[1].asPredicate()
        }

        goals.add(predicateToQuery(pivot))
        return AndQuery(goals.toTypedArray())
    }
    else if (predicate.name == ";") {
        val goals = mutableListOf<Query>()
        goals.add(predicateToQuery(predicate.arguments[0].asPredicate()))

        var pivot = predicate.arguments[1].asPredicate()
        while (pivot.name == ";") {
            goals.add(predicateToQuery(pivot.arguments[0].asPredicate()))
            pivot = pivot.arguments[1].asPredicate()
        }

        goals.add(predicateToQuery(pivot))
        return OrQuery(goals.toTypedArray())
    }
    else {
        return PredicateQuery(predicate)
    }
}

private fun Term.asPredicate(): Predicate {
    if (this is Predicate) return this

    val location = when(this) {
        is ParsedTerm -> this.location
        else -> SourceLocation(SourceUnit("unknown prolog test code"), 0, 0, 0)
    }

    throw ReportingException(SyntaxError("Expected predicate, got $prologTypeName", location))
}