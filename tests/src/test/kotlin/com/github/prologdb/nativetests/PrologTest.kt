package com.github.prologdb.nativetests

import com.github.prologdb.async.LazySequence
import com.github.prologdb.async.LazySequenceBuilder
import com.github.prologdb.async.buildLazySequence
import com.github.prologdb.async.flatMapRemaining
import com.github.prologdb.async.mapRemainingNotNull
import com.github.prologdb.parser.ParseException
import com.github.prologdb.parser.Reporting
import com.github.prologdb.parser.SyntaxError
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.source.SourceLocation
import com.github.prologdb.parser.source.SourceUnit
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment
import com.github.prologdb.runtime.PrologException
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.proofsearch.ASTPrologPredicate
import com.github.prologdb.runtime.proofsearch.ProofSearchContext
import com.github.prologdb.runtime.query.Query
import com.github.prologdb.runtime.term.CompoundTerm
import com.github.prologdb.runtime.term.PrologList
import com.github.prologdb.runtime.term.PrologString
import com.github.prologdb.runtime.term.Term
import com.github.prologdb.runtime.unification.Unification
import com.github.prologdb.runtime.unification.VariableBucket
import io.kotest.assertions.fail
import io.kotest.core.spec.IsolationMode
import io.kotest.core.spec.style.FreeSpec
import org.springframework.core.io.support.PathMatchingResourcePatternResolver
import java.lang.invoke.MethodHandles
import java.nio.file.Path
import java.nio.file.Paths
import java.util.UUID

/** runs the tests found in the *.test.pl files in the prolog tests directory */
class PrologTest : FreeSpec() { init {
    for (prologTestFilePath in prologTestFiles) {
        "${prologTestFilePath.fileName}" - {
            for (testCase in getPrologTestCases(prologTestFilePath)) {
                testCase.name.invoke {
                    testCase.runWith(object : TestResultCallback {
                        override fun onTestSuccess(testName: String) {
                        }

                        override fun onTestFailure(testName: String, message: String) {
                            fail(message)
                        }

                        override fun onTestError(testName: String, error: Throwable) {
                            throw error
                        }

                        override fun onTestParseError(errors: Collection<Reporting>) {
                            errors.forEach(::println)

                            throw RuntimeException("Failed to parse test case file, see STDOUT for errors.")
                        }
                    })
                }
            }
        }
    }
}
    override fun isolationMode() = IsolationMode.InstancePerTest

    private companion object {
        val prologTestFiles: List<Path>
            get() {
                val classLoader = MethodHandles.lookup().javaClass.classLoader
                val resolver = PathMatchingResourcePatternResolver(classLoader)
                return resolver.getResources("classpath:**/*.test.pl")
                    .map { Paths.get(it.file.absolutePath) }
            }

        fun getPrologTestCases(path: Path): Set<PrologTestCase> {
            val moduleRef = TestingModuleLoader.moduleReference(path)
            val runtime = DefaultPrologRuntimeEnvironment(TestingModuleLoader)

            val testModule = try {
                runtime.assureModuleLoaded(moduleRef)
            }
            catch (ex: Exception) {
                return setOf(PrologTestCase.erroring(moduleRef.moduleName, ex))
            }

            return getPrologTestCases(testModule, runtime)
        }

        fun getPrologTestCases(testModule: Module, runtime: DefaultPrologRuntimeEnvironment): Set<PrologTestCase> {
            val by2 = testModule.allDeclaredPredicates[ClauseIndicator.of("by", 2)]
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

                val goalList = arg1.elements.map { it.toQuery() }.toList()

                testCases.add(object : PrologTestCase {
                    override val name = testName

                    override fun runWith(callback: TestResultCallback) {
                        TestExecution(runtime, testModule.declaration.moduleName, testName, goalList).run(callback)
                    }
                })
            }

            return testCases
        }
    }
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

private class TestExecution(private val runtime: DefaultPrologRuntimeEnvironment, val moduleName: String, private val testName: String, private val allGoals: List<Query>) {
    private var failedGoal: Query? = null
    private var stateBeforeFailedGoal: Unification? = null

    private suspend fun LazySequenceBuilder<Unification>.fulfillAllGoals(goals: List<Query>, context: ProofSearchContext,
                                                                         initialVariables: VariableBucket = VariableBucket()): Unification? {
        var sequence = LazySequence.singleton(Unification(initialVariables.copy()))

        for (goalIndex in goals.indices) {
            sequence = sequence.flatMapRemaining { stateBefore ->
                val goalSequence = buildLazySequence<Unification>(principal) {
                    context.fulfillAttach(
                        this,
                        goals[goalIndex].substituteVariables(stateBefore.variableValues),
                        stateBefore.variableValues.copy()
                    )
                }
                val firstSolution = goalSequence.tryAdvance()

                if (firstSolution == null) {
                    failedGoal = goals[goalIndex]
                    stateBeforeFailedGoal = stateBefore
                }

                return@flatMapRemaining yieldAllFinal(
                    buildLazySequence<Unification>(principal) {
                        firstSolution?.let { yield(it) }
                        yieldAllFinal(goalSequence)
                    }
                    .mapRemainingNotNull { goalUnification ->
                        val stateCombined = stateBefore.variableValues.copy()
                        for ((variable, value) in goalUnification.variableValues.values) {
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
                        Unification(stateCombined)
                    }
                )
            }
        }

        return yieldAllFinal(sequence)
    }

    fun run(callback: TestResultCallback) {
        val substitutedGoals = allGoals
            .map { it.substituteVariables(VariableBucket()) }

        val psc = runtime.newProofSearchContext(moduleName)
        val results = buildLazySequence<Unification>(UUID.randomUUID()) {
            fulfillAllGoals(substitutedGoals, psc, VariableBucket())
        }

        try {
            if (results.tryAdvance() != null) {
                callback.onTestSuccess(testName)
            } else {
                var stateStr = ""
                for ((variable, value) in stateBeforeFailedGoal!!.variableValues.values) {
                    val entryTerm = CompoundTerm("=", arrayOf(variable, value))
                    stateStr += entryTerm.toStringUsingOperatorNotations(runtime.getLoadedModule(moduleName).localOperators)
                    stateStr += ",\n"
                }
                stateStr += "\n"
                val goalStr = failedGoal!!.toStringUsingOperatorNotation(runtime.getLoadedModule(moduleName).localOperators)
                callback.onTestFailure(testName, "This goal failed (did not yield a solution):\n$goalStr\n\nState before the goal:\n$stateStr")
            }
        }
        catch (ex: Throwable) {
            if (ex is PrologException) {
                System.err.println(
                    ex.prologStackTrace.joinToString(
                        prefix = "Error in test $testName: ${ex.message}\n  at ",
                        separator = "\n  at ",
                        transform = { it.toString() },
                        postfix = "\n"
                    )
                )
            }
            callback.onTestError(testName, ex)
        }
        finally {
            results.close()
        }
    }
}

private fun Term.toQuery(): Query {
    val result = PrologParser().transformQuery(this)
    ParseException.failOnError(result.reportings)
    return result.item!!
}

private fun Term.asCompound(): CompoundTerm {
    if (this is CompoundTerm) return this

    val location = SourceLocation(
        SourceUnit(sourceInformation.sourceFileName ?: "unknown file"),
        sourceInformation.sourceFileLine ?: 0,
        1,
        1
    )

    throw ParseException.ofSingle(SyntaxError("Expected compound term, got $prologTypeName", location))
}

private class ParseErrorException(val reportings: Collection<Reporting>) : Exception(reportings.firstOrNull()?.run { "$message in $location" })
