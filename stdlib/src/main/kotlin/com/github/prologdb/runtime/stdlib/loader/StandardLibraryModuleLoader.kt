package com.github.prologdb.runtime.stdlib.loader

import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor
import com.github.prologdb.parser.parser.PrologParser
import com.github.prologdb.parser.parser.SourceFileVisitor
import com.github.prologdb.runtime.ClauseIndicator
import com.github.prologdb.runtime.module.Module
import com.github.prologdb.runtime.module.ModuleLoader
import com.github.prologdb.runtime.module.ModuleReference
import com.github.prologdb.runtime.stdlib.BuiltinCompare3
import com.github.prologdb.runtime.stdlib.BuiltinIdentity
import com.github.prologdb.runtime.stdlib.BuiltinNegatedIdentityOperator
import com.github.prologdb.runtime.stdlib.BuiltinNegatedUnity
import com.github.prologdb.runtime.stdlib.BuiltinNot
import com.github.prologdb.runtime.stdlib.BuiltinNotOperator
import com.github.prologdb.runtime.stdlib.BuiltinTermGreaterThan2
import com.github.prologdb.runtime.stdlib.BuiltinTermGreaterThanOrEqual2
import com.github.prologdb.runtime.stdlib.BuiltinTermLessThan2
import com.github.prologdb.runtime.stdlib.BuiltinTermLessThanOrEqual2
import com.github.prologdb.runtime.stdlib.BuiltinUnity
import com.github.prologdb.runtime.stdlib.NativeCodeRule
import com.github.prologdb.runtime.stdlib.dicts.BuiltinGetDict3
import com.github.prologdb.runtime.stdlib.essential.BuiltinAtom1
import com.github.prologdb.runtime.stdlib.essential.BuiltinDecimal1
import com.github.prologdb.runtime.stdlib.essential.BuiltinGround1
import com.github.prologdb.runtime.stdlib.essential.BuiltinInteger1
import com.github.prologdb.runtime.stdlib.essential.BuiltinIsDict1
import com.github.prologdb.runtime.stdlib.essential.BuiltinIsList1
import com.github.prologdb.runtime.stdlib.essential.BuiltinNonGround1
import com.github.prologdb.runtime.stdlib.essential.BuiltinNonVar1
import com.github.prologdb.runtime.stdlib.essential.BuiltinNumber1
import com.github.prologdb.runtime.stdlib.essential.BuiltinString1
import com.github.prologdb.runtime.stdlib.essential.BuiltinTypeof2
import com.github.prologdb.runtime.stdlib.essential.BuiltinVar1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinAbolish1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinAssert1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinRetract1
import com.github.prologdb.runtime.stdlib.essential.clauses.BuiltinRetractAll1
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinApply2
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinCall1
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinColon2
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinCompoundNameArguments3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinCurrentModule1
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinCurrentOp3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinFindAll3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinFindAllOptimized3
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinFindNSols4
import com.github.prologdb.runtime.stdlib.essential.dynamic.BuiltinTermVariables2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinGreaterThan2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinGreaterThanOrEqual2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinIs2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinLessThan2
import com.github.prologdb.runtime.stdlib.essential.math.BuiltinLessThanOrEqual2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinAtomString2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinStringChars2
import com.github.prologdb.runtime.stdlib.essential.string.BuiltinStringCodes2
import com.github.prologdb.runtime.stdlib.lists.BuiltinIota3
import com.github.prologdb.runtime.stdlib.lists.BuiltinIota4
import com.github.prologdb.runtime.stdlib.lists.BuiltinLength2
import com.github.prologdb.runtime.stdlib.lists.BuiltinMember2
import com.github.prologdb.runtime.stdlib.lists.BuiltinSet2
import com.github.prologdb.runtime.stdlib.lists.BuiltinSet3
import com.github.prologdb.runtime.stdlib.lists.BuiltinSort2
import com.github.prologdb.runtime.stdlib.sort.BuiltinPredsort3

object StandardLibraryModuleLoader : ModuleLoader {
    private val _parser = PrologParser()

    private val classPathLoader = ClasspathPrologSourceModuleLoader(
        sourceFileVisitorSupplier = { getSourceFileVisitor(it) },
        classLoader = StandardLibraryModuleLoader::class.java.classLoader,
        parser = _parser
    )

    override fun load(reference: ModuleReference): Module = classPathLoader.load(reference)

    private fun getSourceFileVisitor(moduleReference: ModuleReference): SourceFileVisitor<Module> {
        val nativeImplementations = nativeImplementationsByModuleRef[moduleReference.toString()] ?: emptyMap()
        return NativeCodeSourceFileVisitorDecorator(
            DefaultModuleSourceFileVisitor(),
            nativeImplementations,
            _parser
        )
    }

    private val nativeImplementationsByModuleRef: Map<String, Map<ClauseIndicator, NativeCodeRule>> = mapOf(
        "essential(\$equality)" to listOf(
            BuiltinUnity,
            BuiltinNegatedUnity,
            BuiltinNot,
            BuiltinNotOperator,
            BuiltinIdentity,
            BuiltinNegatedIdentityOperator
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
            BuiltinTypeof2
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
            BuiltinColon2,
            BuiltinCurrentModule1
        ),
        "essential(\$math)" to listOf(
            BuiltinIs2,
            BuiltinLessThan2,
            BuiltinLessThanOrEqual2,
            BuiltinGreaterThan2,
            BuiltinGreaterThanOrEqual2
        ),
        "essential(\$strings)" to listOf(
            BuiltinAtomString2,
            BuiltinStringChars2,
            BuiltinStringCodes2
        ),
        "essential(\$clauses)" to listOf(
            BuiltinAbolish1,
            BuiltinAssert1,
            BuiltinRetract1,
            BuiltinRetractAll1
        ),
        "library(dicts)" to listOf(
            BuiltinGetDict3
        ),
        "library(lists)" to listOf(
            BuiltinIota3,
            BuiltinIota4,
            BuiltinLength2,
            BuiltinMember2,
            BuiltinSet2,
            BuiltinSet3,
            BuiltinSort2
        ),
        "library(sort)" to listOf(
            BuiltinPredsort3
        )
    ).mapValues { (_, nativeCodes) ->
        nativeCodes.associateBy(ClauseIndicator.Companion::of)
    }
}
