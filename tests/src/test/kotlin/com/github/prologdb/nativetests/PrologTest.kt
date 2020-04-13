package com.github.prologdb.nativetests

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.flatMapRemaining
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.parser.ModuleDeclaration
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.ReportingException
import com.github.prologdb.parser.SyntaxError
import com.github.prologdb.parser.lexer.Lexer
import com.github.prologdb.parser.parser.ParseResult
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.builtin.ISOOpsOperatorRegistry
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.query.AndQuery
import com.github.prologdb.runtime.query.OrQuery
import com.github.prologdb.runtime.query.PredicateInvocationQuery
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import com.github.prologdb.runtime.util.DefaultOperatorRegistry
import com.github.prologdb.runtime.util.OperatorDefinition
import com.github.prologdb.runtime.util.OperatorType
import io.kotlintest.matchers.fail
import io.kotlintest.matchers.shouldEqual
import io.kotlintest.specs.FreeSpec
import org.springframework.core.io.support.PathMatchingResourcePatternResolver
import java.lang.invoke.MethodHandles
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.UUID

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

        val testModule = parseResult.item ?: throw RuntimeException("Invalid return value from parser: is success but item is null")
        val testCases = getPrologTestCases(testModule)

        testCases.forEach { it.runWith(callback) }
    }

    private fun parseFile(path: Path): ParseResult<Module> {
        val fileContent = String(Files.readAllBytes(path), Charset.forName("UTF-8"))
        val sourceUnit = SourceUnit(path.toString())
        val lexer = Lexer(fileContent.iterator(), SourceLocation(sourceUnit, 1, 0, 0))

        return PrologParser().parseModule(
            lexer,
            TestingOperatorRegistry,
            ModuleDeclaration(path.toString(), null)
        )
    }

    private fun getPrologTestCases(testModule: Module): Set<PrologTestCase> {
        val by2 = testModule.exportedPredicates[ClauseIndicator.of("by", 2)]
            ?: return emptySet()

        if (by2 !is ASTPrologPredicate) {
            throw IllegalStateException("Who the heck parsed this? predicate by/2 from test module is a ${by2.javaClass.name}, expected ${ASTPrologPredicate::class.simpleName}")
        }

        val testCases = mutableSetOf<PrologTestCase>()
        for (by2instance in by2.clauses) {
            if (by2instance !is CompoundTerm) continue

            val arg0 = by2instance.arguments[0]
            val arg1 = by2instance.arguments[1]
            if (arg0 !is CompoundTerm) continue
            if (arg1 !is PrologList) continue

            if (arg0.functor != "test") continue
            if (arg0.arity != 1) continue
            if (arg0.arguments[0] !is PrologString) continue

            val testName = (arg0.arguments[0] as PrologString).toKotlinString()

            val goalList = arg1.elements.map { it.asCompound().toQuery() }.toList()

            testCases.add(object : PrologTestCase {
                override val name = testName

                override fun runWith(callback: TestResultCallback) {
                    val runtimeEnv = PrologRuntimeEnvironment(testModule)
                    TestExecution(runtimeEnv, testName, goalList).run(callback)
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

        return resolver.getResources("classpath:**/*.test.pl")
            .map { Paths.get(it.file.absolutePath) }
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

private val TestingOperatorRegistry = DefaultOperatorRegistry().apply {
    include(ISOOpsOperatorRegistry)
    defineOperator(OperatorDefinition(100, OperatorType.FX, "test"))
    defineOperator(OperatorDefinition(800, OperatorType.XFX, "by"))
}

private class TestExecution(private val runtime: PrologRuntimeEnvironment, private val testName: String, private val allGoals: List<Query>) {
    private var failedGoal: Query? = null

    private suspend fun LazySequenceBuilder<Unification>.fulfillAllGoals(goals: List<Query>, context: ProofSearchContext,
                                                                         initialVariables: VariableBucket = VariableBucket()): Unification? {
        var sequence = LazySequence.singleton(Unification(initialVariables.copy()))

        for (goalIndex in goals.indices) {
            sequence = sequence.flatMapRemaining { stateBefore ->
                val goalSequence = buildLazySequence<Unification>(principal) {
                    val lastSolution = context.fulfillAttach(
                        this,
                        goals[goalIndex].substituteVariables(stateBefore.variableValues),
                        stateBefore.variableValues.copy()
                    )

                    if (lastSolution == null) {
                        failedGoal = goals[goalIndex]
                    }

                    lastSolution
                }
                return@flatMapRemaining yieldAllFinal(goalSequence.mapRemainingNotNull { goalUnification ->
                    val stateCombined = stateBefore.variableValues.copy()
                    for ((variable, value) in goalUnification.variableValues.values) {
                        if (value != null) {
                            // substitute all instantiated variables for simplicity and performance
                            val substitutedValue = value.substituteVariables(stateCombined.asSubstitutionMapper())
                            if (stateCombined.isInstantiated(variable)) {
                                if (stateCombined[variable] != substitutedValue && stateCombined[variable] != value) {
                                    // instantiated to different value => no unification
                                    failedGoal = goals[goalIndex]
                                    return@mapRemainingNotNull null
                                }
                            }
                            else {
                                stateCombined.instantiate(variable, substitutedValue)
                            }
                        }
                    }
                    Unification(stateCombined)
                })
            }
        }

        return yieldAllFinal(sequence)
    }

    fun run(callback: TestResultCallback) {
        val substitutedGoals = allGoals
            .map { it.substituteVariables(VariableBucket()) }

        val psc = runtime.newProofSearchContext()
        val results = buildLazySequence<Unification>(UUID.randomUUID()) {
            fulfillAllGoals(substitutedGoals, psc, VariableBucket())
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

private fun CompoundTerm.toQuery(): Query {
    if (this.functor == ",") {
        val goals = mutableListOf<Query>()
        goals.add(this.arguments[0].asCompound().toQuery())

        var pivot = this.arguments[1].asCompound()
        while (pivot.functor == ",") {
            goals.add(pivot.arguments[0].asCompound().toQuery())
            pivot = pivot.arguments[1].asCompound()
        }

        goals.add(pivot.toQuery())
        return AndQuery(goals.toTypedArray()).also { it.sourceInformation = this.sourceInformation }
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
        return OrQuery(goals.toTypedArray()).also { it.sourceInformation = this.sourceInformation }
    }
    else {
        return PredicateInvocationQuery(this).also { it.sourceInformation = this.sourceInformation }
    }
}

private fun Term.asCompound(): CompoundTerm {
    if (this is CompoundTerm) return this

    val location = SourceLocation(
        SourceUnit(sourceInformation.sourceFileName ?: "unknown file"),
        sourceInformation.sourceFileLine ?: 0,
        1,
        1
    )

    throw ReportingException(SyntaxError("Expected compound term, got $prologTypeName", location))
}
