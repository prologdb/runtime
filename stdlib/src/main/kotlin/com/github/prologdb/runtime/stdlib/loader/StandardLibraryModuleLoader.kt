package com.github.prologdb.runtime.stdlib.loader

import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.PrologRuntimeEnvironment
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.proofsearch.Rule
import com.github.prologdb.runtime.stdlib.aggregate.BuiltinReduce2
import com.github.prologdb.runtime.stdlib.dicts.BuiltinDictPairs2
import com.github.prologdb.runtime.stdlib.dicts.BuiltinGetDict3
import com.github.prologdb.runtime.stdlib.essential.*
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinAssert1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinError1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinRetract1
import com.github.prologdb.runtime.stdlib.essential.dynamic.*
import com.github.prologdb.runtime.stdlib.essential.math.*
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinAtomString2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinStringChars2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinStringCodes2
import com.github.prologdb.runtime.stdlib.lists.*
import com.github.prologdb.runtime.stdlib.solution_sequences.*
import com.github.prologdb.runtime.stdlib.sort.BuiltinPredsort3

class StandardLibraryModuleLoaderServiceLoaderProxy : ModuleLoader by StandardLibraryModuleLoader

object StandardLibraryModuleLoader : ModuleLoader {
    private val _parser = PrologParser()
    private val classpathPrefix = "com/github/prologdb/runtime/stdlib"

    private val classPathLoader = ClasspathPrologSourceModuleLoader(
        sourceFileVisitorSupplier = this::getSourceFileVisitor,
        classLoader = StandardLibraryModuleLoader::class.java.classLoader,
        parser = _parser,
        moduleReferenceToClasspathPath = { moduleRef ->
            "$classpathPrefix/${moduleRef.pathAlias}/${moduleRef.moduleName}.pl"
        }
    )

    override fun initiateLoading(reference: ModuleReference, runtime: PrologRuntimeEnvironment): ModuleLoader.PrimedStage = classPathLoader.initiateLoading(reference, runtime)

    private fun getSourceFileVisitor(moduleReference: ModuleReference, runtime: PrologRuntimeEnvironment): SourceFileVisitor<Module> {
        val nativeImplementations = nativeImplementationsByModuleRef[moduleReference.toString()] ?: emptyMap()
        return NativeCodeSourceFileVisitorDecorator(
            DefaultModuleSourceFileVisitor(runtime),
            nativeImplementations,
            _parser
        )
    }

    private val nativeImplementationsByModuleRef: Map<String, Map<ClauseIndicator, Rule>> = mapOf(
        "essential(\$equality)" to listOf(
            BuiltinUnity,
            BuiltinNegatedUnity,
            BuiltinNot,
            BuiltinNotOperator,
            BuiltinIdentity,
            BuiltinNegatedIdentityOperator,
            BuiltinVariance,
            BuiltinNegatedVariance,
        ),
        "essential(\$typesafety)" to listOf(
            BuiltinAtom1,
            BuiltinInteger1,
            BuiltinDecimal1,
            BuiltinNumber1,
            BuiltinString1,
            BuiltinIsList1,
            BuiltinIsDict1,
            BuiltinVar1,
            BuiltinNonVar1,
            BuiltinGround1,
            BuiltinNonGround1,
            BuiltinTypeof2,
            BuiltinRequire2,
        ),
        "essential(\$comparison)" to listOf(
            BuiltinTermLessThan2,
            BuiltinTermLessThanOrEqual2,
            BuiltinTermGreaterThan2,
            BuiltinTermGreaterThanOrEqual2,
            BuiltinCompare3
        ),
        "essential(\$dynamic)" to listOf(
            BuiltinFindAll3,
            BuiltinFindAllOptimized3,
            BuiltinFindNSols4,
            BuiltinCall1,
            BuiltinCompoundNameArguments3,
            BuiltinApply2,
            BuiltinTermVariables2,
            BuiltinCurrentOp3,
            BuiltinCurrentModule1,
            BuiltinQualifyCallable3,
        ),
        "essential(\$math)" to listOf(
            BuiltinIs2,
            BuiltinLessThan2,
            BuiltinLessThanOrEqual2,
            BuiltinGreaterThan2,
            BuiltinGreaterThanOrEqual2,
            BuiltinNumericNotEqual2,
        ),
        "essential(\$strings)" to listOf(
            BuiltinAtomString2,
            BuiltinStringChars2,
            BuiltinStringCodes2
        ),
        "essential(\$clauses)" to listOf(
            BuiltinAssert1,
            BuiltinRetract1,
            BuiltinError1,
        ),
        "library(dicts)" to listOf(
            BuiltinGetDict3,
            BuiltinDictPairs2,
        ),
        "library(lists)" to listOf(
            BuiltinLength2,
            BuiltinMember2,
            BuiltinSet2,
            BuiltinSet3,
            BuiltinSort2,
            BuiltinReverse2,
            BuiltinMSort2,
        ),
        "library(sort)" to listOf(
            BuiltinPredsort3
        ),
        "library(solution_sequences)" to listOf(
            BuiltinLimit2,
            BuiltinOffset2,
            BuiltinDistinct2,
            BuiltinCallNth2,
            BuiltinGroupBy4,
        ),
        "library(aggregate)" to listOf(
            BuiltinReduce2,
        ),
    ).mapValues { (_, nativeCodes) ->
        nativeCodes.associateBy(ClauseIndicator.Companion::of)
    }
}
