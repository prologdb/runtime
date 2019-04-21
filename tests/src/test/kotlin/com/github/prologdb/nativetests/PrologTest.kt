package com.github.prologdb.nativetests

import com.github.prologdb.async.*
import com.github.prologdb.parser.*
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.HasPrologSource
import com.github.prologdb.runtime.NullSourceInformation
import com.github.prologdb.runtime.RandomVariableScope
import com.github.prologdb.runtime.knowledge.KnowledgeBase
import com.github.prologdb.runtime.knowledge.ProofSearchContext
import com.github.prologdb.runtime.knowledge.ReadWriteAuthorization
import com.github.prologdb.runtime.knowledge.library.Library
import com.github.prologdb.runtime.knowledge.library.OperatorDefinition
import com.github.prologdb.runtime.knowledge.library.OperatorType
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.*
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import io.kotlintest.matchers.fail
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.specs.FreeSpec
import org.springframework.core.io.support.PathMatchingResourcePatternResolver
import java.lang.invoke.MethodHandles
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.*

/** runs the tests found in the *.test.pl files in the prolog tests directory */
class PrologTest : FreeSpec() { init {
    for (prologTestFilePath in prologTestFiles) {
        "${prologTestFilePath.fileName}" - {
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

            runPrologTestFile(prologTestFilePath, callback)
        }
    }
}
    private fun runPrologTestFile(path: Path, callback: TestResultCallback) {
        val parseResult = parseFile(path)

        if (!parseResult.isSuccess || parseResult.reportings.isNotEmpty()) {
            callback.onTestParseError(parseResult.reportings)
            return
        }

        val library = parseResult.item ?: throw RuntimeException("Invalid return value from parser: is success but item is null")
        val testCases = getPrologTestCases(library, callback::onTestParseError)

        testCases.forEach { it.runWith(callback) }
    }

    private fun parseFile(path: Path): ParseResult<Library> {
        val fileContent = String(Files.readAllBytes(path), Charset.forName("UTF-8"))
        val sourceUnit = SourceUnit(path.toString())
        val lexer = Lexer(fileContent.iterator(), SourceLocation(sourceUnit, 1, 0, 0))

        return PrologParser().parseLibrary(path.fileName.toString(), lexer, createFreshTestingKnowledgeBase().operators)
    }

    private fun getPrologTestCases(library: Library, parseErrorCallback: (Collection<Reporting>) -> Any?): Set<PrologTestCase> {
        val by2Results = library.findFor(CompoundTerm("by", arrayOf(AnonymousVariable, AnonymousVariable)))
        val testCases = mutableSetOf<PrologTestCase>()
        for (by2instance in by2Results) {
            if (by2instance !is CompoundTerm) continue
            val arg0 = by2instance.arguments[0]
            val arg1 = by2instance.arguments[1]
            if (arg0 !is CompoundTerm) continue
            if (arg1 !is PrologList) continue

            if (arg0.functor != "test") continue
            if (arg0.arity != 1) continue
            if (arg0.arguments[0] !is PrologString) continue

            val testName = (arg0.arguments[0] as PrologString).toKotlinString()

            if (by2instance !is ParsedCompoundTerm) {
                testCases.add(PrologTestCase.erroring(testName, IllegalArgumentException("Test cases must be constructed from parsed code so failure locations can be reported.")))
            }

            val goalList = (arg1 as ParsedList).elements.map { it.asCompound().toQuery() }.toList()

            testCases.add(object : PrologTestCase {
                override val name = testName

                override fun runWith(callback: TestResultCallback) {
                    val runtimeEnv = LocalKnowledgeBase.createWithDefaults()
                    runtimeEnv.load(library)
                    TestExecution(runtimeEnv, IrrelevantPrincipal, testName, goalList).run(callback)
                }
            })
        }

        return testCases
    }
}

private val prologTestFiles: List<Path>
    get() {
        val classLoader = MethodHandles.lookup().javaClass.classLoader
        val resolver = PathMatchingResourcePatternResolver(classLoader)

        return resolver.getResources("classpath:**/*.test.pl").map { Paths.get(it.file.absolutePath) }
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

    companion object {
        fun erroring(name: String, error: Throwable): PrologTestCase = object : PrologTestCase {
            override val name: String = name

            override fun runWith(callback: TestResultCallback) {
                callback.onTestError(this.name, error)
            }
        }
    }
}

private fun createFreshTestingKnowledgeBase(): LocalKnowledgeBase {
    val kb = LocalKnowledgeBase.createWithDefaults()
    kb.defineOperator(OperatorDefinition(100, OperatorType.FX, "test"))
    kb.defineOperator(OperatorDefinition(800, OperatorType.XFX, "by"))

    return kb
}

private fun KnowledgeBase.defineOperator(def: OperatorDefinition) {
    invokeDirective("op", ReadWriteAuthorization, arrayOf(
        PrologInteger(def.precedence.toLong()),
        Atom(def.type.name.toLowerCase()),
        Atom(def.name))
    ).consumeAll()
}

private class TestExecution(private val withinKB: LocalKnowledgeBase, private val principal: Principal, private val testName: String, private val allGoals: List<Query>) {
    private var failedGoal: Query? = null

    private suspend fun LazySequenceBuilder<Unification>.fulfillAllGoals(goals: List<Query>, context: ProofSearchContext,
                                                                 vars: VariableBucket = VariableBucket()) {
        val goal = goals[0].substituteVariables(vars)

        var goalHadAnySolutions = false
        buildLazySequence<Unification>(context.principal) {
            context.fulfillAttach(this, goal, VariableBucket())
        }
            .forEachRemaining { goalUnification ->
                goalHadAnySolutions = true
                val goalVars = vars.copy()
                for ((variable, value) in goalUnification.variableValues.values) {
                    if (value != null) {
                        // substitute all instantiated variables for simplicity and performance
                        val substitutedValue = value.substituteVariables(goalVars.asSubstitutionMapper())
                        if (goalVars.isInstantiated(variable)) {
                            if (goalVars[variable] != substitutedValue && goalVars[variable] != value) {
                                // instantiated to different value => no unification
                                return@forEachRemaining
                            }
                        }
                        else {
                            goalVars.instantiate(variable, substitutedValue)
                        }
                    }
                }

                if (goals.size == 1) {
                    // this was the last goal in the list and it is fulfilled
                    // the variable bucket now holds all necessary instantiations
                    yield(Unification(goalVars))
                }
                else {
                    fulfillAllGoals(goals.subList(1, goals.size), context, goalVars)
                }
            }

        if (!goalHadAnySolutions) {
            failedGoal = allGoals[allGoals.size - goals.size]
        }
    }

    fun run(callback: TestResultCallback) {
        val substitutedGoals = allGoals
            .map { it.substituteVariables(VariableBucket()) }

        val results = buildLazySequence(UUID.randomUUID()) {
            fulfillAllGoals(substitutedGoals, TestQueryContext(principal, withinKB), VariableBucket())
        }

        try {
            if (results.tryAdvance() != null) {
                callback.onTestSuccess(testName)
            } else {
                callback.onTestFailure(testName, "Goal ${failedGoal!!} failed (did not yield a solution).")
            }
        }
        catch (ex: Throwable) {
            callback.onTestError(testName, ex)
        }
        finally {
            results.close()
        }
    }
}

private class TestQueryContext(
    override val principal: Principal,
    private val knowledgeBase: LocalKnowledgeBase
) : ProofSearchContext {

    override val authorization = ReadWriteAuthorization

    override val randomVariableScope = RandomVariableScope()

    override val fulfillAttach: suspend LazySequenceBuilder<Unification>.(Query, VariableBucket) -> Unit = { q, v ->
        assert(v.isEmpty)

        knowledgeBase.fulfill(q, ReadWriteAuthorization, randomVariableScope).forEachRemaining {
            yield(it)
        }
    }
}

private fun ParsedCompoundTerm.toQuery(): Query {
    if (this.functor == ",") {
        val goals = mutableListOf<Query>()
        goals.add(this.arguments[0].asCompound().toQuery())

        var pivot = this.arguments[1].asCompound()
        while (pivot.functor == ",") {
            goals.add(pivot.arguments[0].asCompound().toQuery())
            pivot = pivot.arguments[1].asCompound()
        }

        goals.add(pivot.toQuery())
        return ParsedAndQuery(goals.toTypedArray(), this.sourceInformation)
    }
    else if (this.functor == ";") {
        val goals = mutableListOf<Query>()
        goals.add(this.arguments[0].asCompound().toQuery())

        var pivot = this.arguments[1].asCompound()
        while (pivot.functor == ";") {
            goals.add(pivot.arguments[0].asCompound().toQuery())
            pivot = pivot.arguments[1].asCompound()
        }

        goals.add(pivot.toQuery())
        return ParsedOrQuery(goals.toTypedArray(), this.sourceInformation)
    }
    else {
        return ParsedPredicateInvocationQuery(this)
    }
}

private fun Term.asCompound(): ParsedCompoundTerm {
    if (this is ParsedCompoundTerm) return this

    val sourceInformation = if (this is HasPrologSource) this.sourceInformation else NullSourceInformation
    val location = SourceLocation(
        SourceUnit(sourceInformation.sourceFileName ?: "unknown file"),
        sourceInformation.sourceFileLine ?: 0,
        1,
        1
    )

    throw ReportingException(SyntaxError("Expected compound term, got $prologTypeName", location))
}
